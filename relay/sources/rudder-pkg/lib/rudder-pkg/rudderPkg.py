"""
    Contains functions called by the parser and nothing else.
"""
import sys
import os
import io
import re
import shutil
import requests as requests
import logging
from tabulate import tabulate
import plugin
import rpkg
import rudderPkgUtils as utils
from lxml import html


"""
    Expect a list of path as parameter.
    Try to install the given rpkgs.
"""
def install_file(package_files):
    for package_file in package_files:
        logging.info("Installing " + package_file)
        # First, check if file exists
        if not os.path.isfile(package_file):
            utils.fail("Error: Package file " + package_file + " does not exist")
        metadata = utils.rpkg_metadata(package_file)
        exist = utils.package_check(metadata)
        # As dependencies are only displayed messages for now,
        # wait until the end to make them visible.
        # This should be moved before actual installation once implemented.
        if not utils.install_dependencies(metadata):
            exit(1)
        if exist:
            logging.info("The package is already installed, I will upgrade it.")
        script_dir = utils.extract_scripts(metadata, package_file)
        utils.run_script("preinst", script_dir, exist)
        utils.install(metadata, package_file, exist)
        utils.run_script("postinst", script_dir, exist)
        if metadata['type'] == 'plugin' and 'jar-files' in metadata:
            for j in metadata['jar-files']:
                utils.jar_status(j, True)

"""
    List installed plugins.
"""
def package_list_installed():
    toPrint = []
    printLatest = os.path.isfile(utils.INDEX_PATH)

    for p in utils.DB["plugins"].keys():
        currentVersion = rpkg.PluginVersion(utils.DB["plugins"][p]["version"])
        extra = ""
        if printLatest:
            pkgs = plugin.Plugin(p)
            pkgs.getAvailablePackages()
            latestVersion = pkgs.getLatestCompatibleRelease().version
            if currentVersion < latestVersion:
                extra = "version %s is available"%(latestVersion.pluginLongVersion)
            toPrint.append([p, currentVersion.pluginLongVersion, latestVersion.pluginLongVersion + " " + extra])
        else:
            toPrint.append([p, currentVersion.pluginLongVersion])

    if printLatest:
        print(tabulate(toPrint, headers=['Plugin Name', 'Version', 'Latest release'], tablefmt='orgtbl'))
    else:
        print(tabulate(toPrint, headers=['Plugin Name', 'Version'], tablefmt='orgtbl'))

"""
    List available plugin names.
"""
def package_list_name():
    pluginDict = utils.list_plugin_name()
    toPrint = []
    for p in pluginDict.keys():
        toPrint.append([p, pluginDict[p][0], pluginDict[p][1]])
    print(tabulate(toPrint, headers=['Plugin Name', 'Plugin Short Name', 'Description'], tablefmt='orgtbl'))

"""
    Given a name, a version, and a mode, print associated plugin metadata.
    If no version is given it will take the latest version in the given mode.
"""
def package_show(name, version, mode):
    utils.readConf()
    pkgs = plugin.Plugin(name[0])
    pkgs.getAvailablePackages()
    if version != "":
        if mode == "release":
            rpkg = pkgs.getRpkgByLongVersion(version, mode)
        else:
            rpkg = pkgs.getRpkgByLongVersion(version, mode)
    else:
        if mode == "release":
            rpkg = pkgs.getLatestCompatibleRelease()
        else:
            rpkg = pkgs.getLatestCompatibleNightly()

    rpkg.show_metadata()

"""
    Given a name, lookf for a the given packages availables for this plugin.
"""
def package_search(name):
    utils.readConf()
    pkgs = plugin.Plugin(name[0])
    pkgs.getAvailablePackages()
    toPrint = []
    for iRpkg in sorted(pkgs.packagesInfo):
        toPrint.append(iRpkg.toTabulate())
    print(tabulate(toPrint, headers=['Name', 'release mode', 'Version', 'Compatible'], tablefmt='orgtbl'))

"""
    Install the package for a given plugin in a specific version.
    It will not check for compatibility and will let it to the installer since
    the user explicitly asked for this version.
"""
def package_install_specific_version(name, longVersion, mode="release"):
    utils.readConf()
    pkgs = plugin.Plugin(name[0])
    pkgs.getAvailablePackages()
    if mode == "release":
        rpkg = pkgs.getRpkgByLongVersion(longVersion, mode)
    else:
        rpkg = pkgs.getRpkgByLongVersion(longVersion, mode)
    rpkgPath = utils.downloadByRpkg(rpkg)
    install_file([rpkgPath])

"""
    Install the latest available and compatible package for a given plugin.
    If no release mode is given, it will only look in the released rpkg.
"""
def package_install_latest(name, mode="release"):
    utils.readConf()
    pkgs = plugin.Plugin(name[0])
    pkgs.getAvailablePackages()
    if mode == "release":
        rpkg = pkgs.getLatestCompatibleRelease()
    else:
        rpkg = pkgs.getLatestCompatibleNightly()
    rpkgPath = utils.downloadByRpkg(rpkg)
    install_file([rpkgPath])

"""Remove a given plugin. Expect a list of name as parameter."""
def remove(package_names):
    for package_name in package_names:
        logging.info("Removing " + package_name)
        if package_name not in utils.DB["plugins"]:
            utils.fail("This package is not installed. Aborting!", 2)
        script_dir = utils.DB_DIRECTORY + "/" + package_name
        metadata = utils.DB["plugins"][package_name]
        if metadata['type'] == 'plugin' and 'jar-files' in metadata:
            for j in metadata['jar-files']:
                utils.jar_status(j, False)
        utils.run_script("prerm", script_dir, None)
        utils.remove_files(metadata)
        utils.run_script("postrm", script_dir, None)
        shutil.rmtree(script_dir)
        del utils.DB["plugins"][package_name]
        utils.db_save()

def rudder_postupgrade():
    for plugin in utils.DB["plugins"]:
        script_dir = utils.DB_DIRECTORY + "/" + plugin
        utils.run_script("postinst", script_dir, True)

def check_compatibility():
    for p in utils.DB["plugins"]:
        metadata = utils.DB["plugins"][p]
        if not utils.check_plugin_compatibility(metadata):
            logging.warning("Plugin " + p + " is not compatible with rudder anymore, disabling it.")
            if 'jar-files' in metadata:
                for j in metadata['jar-files']:
                    utils.jar_status(j, False)
            logging.warning("Please install a new version of " + p + " to enable it again.")
            logging.info("")
            utils.jetty_needs_restart = True

def plugin_save_status():
    enabled = []
    if not os.path.exists(utils.PLUGINS_CONTEXT_XML):
        return
    text = open(utils.PLUGINS_CONTEXT_XML).read()
    match = re.search(r'<Set name="extraClasspath">(.*?)</Set>', text)
    if match:
        enabled = match.group(1).split(',')
    for p in utils.DB["plugins"]:
        metadata = utils.DB["plugins"][p]
        if 'jar-files' in metadata:
            for j in metadata['jar-files']:
                if j in enabled:
                    print("enabled " + j)
                else:
                    print("disabled " + j)

def plugin_restore_status():
    lines = sys.stdin.readlines()
    for line in lines:
        line = line.strip()
        if line.startswith("enabled "):
            print("enable " + line.split(' ')[1])
            utils.jar_status(line.split(' ')[1], True)
        if line.startswith("disabled "):
            utils.jar_status(line.split(' ')[1], False)
    check_compatibility()

def plugin_status(plugins, status):
    for plugin in plugins:
        if status:
            print("Enabling " + plugin)
        else:
            print("Disabling " + plugin)
        if plugin not in utils.DB["plugins"]:
            utils.fail("Unknown plugin " + plugin)
        metadata = utils.DB["plugins"][plugin]
        if 'jar-files' in metadata:
            for j in metadata['jar-files']:
                utils.jar_status(j, status)

def plugin_disable_all():
    plugin_status(utils.DB["plugins"].keys(), False)

def plugin_enable_all():
    plugin_status(utils.DB["plugins"].keys(), True)

def update_licenses():
    utils.readConf()
    url = utils.URL + "/licenses/" + utils.REPO
    r = requests.get(url, auth=(utils.USERNAME, utils.PASSWORD))
    htmlElements = html.document_fromstring(r.text)
    htmlElements.make_links_absolute(url + "/", resolve_base_href=True)
    toDownload = re.compile('\S+\.(license|key)')

    for elem in htmlElements.iterlinks():
        match = toDownload.search(elem[2])
        if match is not None:
            #logging.info("downloading %s"%(elem[2]))
            utils.download(elem[2], utils.LICENCES_PATH + "/" + os.path.basename(elem[2]))

# TODO validate index sign if any?
""" Download the index file on the repos """
def update():
    utils.readConf()
    logging.debug('Updating the index')
    utils.getRudderKey()
    # backup the current indexFile if it exists
    logging.debug("backuping %s in %s"%(utils.INDEX_PATH, utils.INDEX_PATH + ".bkp"))
    if os.path.isfile(utils.INDEX_PATH):
        os.rename(utils.INDEX_PATH, utils.INDEX_PATH + ".bkp")
    try:
        utils.download(utils.URL + "/" + "rpkg.index")
    except Exception as e:
        if os.path.isfile(utils.INDEX_PATH + ".bkp"):
            logging.debug("restoring %s from %s"%(utils.INDEX_PATH, utils.INDEX_PATH + ".bkp"))
            os.rename(utils.INDEX_PATH + ".bkp", utils.INDEX_PATH)
        utils.fail(e)
