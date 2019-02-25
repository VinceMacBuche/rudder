use super::context::VarContext;
use super::enums::EnumList;
use super::value::Value;
use super::resource::Parameter;
use crate::error::*;
use crate::parser::*;
use std::collections::HashMap;

/// PreAST is a structure that looks like ans AST but is not an AST.
/// We need all global data to create the final AST.
/// So we store them in a PreAST and create the final AST once we have everything.
#[derive(Debug)]
pub struct PreAST<'src> {
    pub enum_list: EnumList<'src>,
    pub enum_mapping: Vec<PEnumMapping<'src>>,
    pub pre_resources: HashMap<Token<'src>, PreResources<'src>>,
    pub variables: VarContext<'src>,
    pub parameter_defaults: // global list of parameter defaults
    //           resource,    state,               default values
        HashMap<(Token<'src>, Option<Token<'src>>), Vec<Option<Value<'src>>>>,
}

/// PreResource is a temporary Resource structure for PreAST
#[derive(Debug)]
pub struct PreResources<'src> {
    pub metadata: HashMap<Token<'src>, Value<'src>>,
    pub parameters: Vec<Parameter<'src>>,
    //                   metadata                           state
    pub pre_states: Vec<(HashMap<Token<'src>, Value<'src>>, PStateDef<'src>)>,
}

impl<'src> PreAST<'src> {
    pub fn new() -> PreAST<'static> {
        PreAST {
            enum_list: EnumList::new(),
            enum_mapping: Vec::new(),
            pre_resources: HashMap::new(),
            variables: VarContext::new(),
            parameter_defaults: HashMap::new(),
        }
    }

    /// Add a file parsed with the top level parser.
    /// Call this once for each file before creating the AST.
    /// Uniqueness checks are done here.
    pub fn add_parsed_file(&mut self, filename: &'src str, file: PFile<'src>) -> Result<()> {
        if file.header.version != 0 {
            panic!("Multiple format not supported yet");
        }
        let mut current_metadata: HashMap<Token<'src>, PValue<'src>> = HashMap::new();
        // iterate over all parsed declarations
        fix_results(file.code.into_iter().map(|decl| {
            match decl {

                // comments are concatenated and are considered metadata.
                PDeclaration::Comment(c) => {
                    if current_metadata.contains_key(&Token::new("comment", filename)) {
                        current_metadata
                            .entry(Token::new("comment", filename))
                            .and_modify(|e| {
                                *e = match e {
                                    PValue::String(tag, st) => {
                                        PValue::String(*tag, st.to_string() + c)
                                    }
                                }
                            });
                    } else {
                        current_metadata.insert("comment".into(), PValue::String(c, c.to_string()));
                    }
                }

                // Metadata are stored into a hashmap for later use.
                PDeclaration::Metadata(m) => {
                    // metadata must not be called "comment"
                    if m.key == Token::from("comment") {
                        fail!(m.key, "Metadata name '{}' is forbidden", m.key);
                    }
                    if current_metadata.contains_key(&m.key) {
                        fail!(
                            m.key,
                            "Metadata name '{}' is already defined at {}",
                            m.key,
                            current_metadata.entry(m.key).key()
                        );
                    }
                    current_metadata.insert(m.key, m.value);
                }

                // Resource declaration are stored in a temporary format (PreResource) with their metadata.
                // Parameter defaults are processed because they are needed during AST creation.
                PDeclaration::Resource(rd) => {
                    let PResourceDef {
                        name,
                        parameters,
                        parameter_defaults,
                    } = rd;
                    if self.pre_resources.contains_key(&name) {
                        fail!(
                            name,
                            "Resource {} has already been defined in {}",
                            name,
                            self.pre_resources.entry(name).key()
                        );
                    }
                    let metadata = fix_map_results(
                        current_metadata
                            .drain() // Move the content without moving the structure
                            .map(|(k, v)| Ok((k, Value::from_pvalue(v)?))),
                    )?;
                    let param_defaults = fix_vec_results(
                        parameter_defaults.into_iter().map(|p|
                            Ok(match p {
                                Some(x) => Some(Value::from_pvalue(x)?),
                                None => None,
                            })
                        )
                    )?;
                    let resource = PreResources {
                        metadata,
                        parameters: fix_vec_results(parameters
                            .into_iter()
                            .zip(param_defaults.iter())
                            .map(|(p,d)| Parameter::from_pparameter(p,d) )
                        )?,
                        pre_states: Vec::new(),
                    };
                    // store resource declaration
                    self.pre_resources.insert(name, resource);
                    // default values are stored in a separate structure
                    self.parameter_defaults.insert((name, None), param_defaults);
                    // Reset metadata
                    current_metadata = HashMap::new();
                }

                // State declaration are stored in the same format with their metadata.
                // State are not checked for uniqueness here but n AST creation.
                // Parameter defaults are processed because they are needed during AST creation.
                PDeclaration::State(st) => {
                    let PStateDef {
                        name,
                        resource_name,
                        parameters,
                        parameter_defaults,
                        statements,
                    } = st;
                    let param_defaults = fix_vec_results(
                        parameter_defaults.into_iter().map(|p|
                            Ok(match p {
                                Some(x) => Some(Value::from_pvalue(x)?),
                                None => None,
                            })
                        )
                    )?;
                    self.parameter_defaults.insert((resource_name, Some(name)), param_defaults);
                    if let Some(rd) = self.pre_resources.get_mut(&resource_name) {
                        let metadata = fix_map_results(
                            current_metadata
                                .drain() // Move the content without moving the structure
                                .map(|(k, v)| Ok((k, Value::from_pvalue(v)?))),
                        )?;
                        rd.pre_states.push((
                            metadata,
                            PStateDef {
                                name,
                                resource_name,
                                parameters,
                                parameter_defaults: Vec::new(),
                                statements,
                            },
                        ));
                        // Reset metadata
                        current_metadata = HashMap::new();
                    } else {
                        fail!(
                            resource_name,
                            "Resource {} has not been defined for {}",
                            resource_name,
                            name
                        );
                    }
                }

                // Enums are fully processed here. All the code is in enum.rs
                PDeclaration::Enum(e) => {
                    // Metadata not supported on enums
                    if !current_metadata.is_empty() {
                        fail!(
                            e.name,
                            "Metadata and documentation comment not supported on enums yet (in {})",
                            e.name
                        )
                    }
                    if e.global {
                        self.variables
                            .new_enum_variable(None, e.name, e.name, None)?;
                    }
                    self.enum_list.add_enum(e)?;
                }

                // Enum mappings are fully processed here. All the code is in enum.rs
                PDeclaration::Mapping(em) => {
                    // Metadata not supported on enums
                    if !current_metadata.is_empty() {
                        fail!(
                            &em.to,
                            "Metadata and documentation comment not supported on enums yet (in {})",
                            &em.to
                        )
                    }
                    self.enum_mapping.push(em);
                }

                // Global variables are put in a global context for typing
                // and stored as a global declaration for code generation.
                PDeclaration::GlobalVar(variable,value) => {
                    let val = Value::from_pvalue(value)?;
                    self.variables.new_constant(None, variable, val.get_type(), val)?;

                }

            };
            Ok(())
        }))
    }
}

// TESTS
//

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_string(input: &str) -> Result<()> {
        let mut pre_ast = PreAST::new();
        let content = parse_file("test_string", input).unwrap();
        pre_ast.add_parsed_file("test_string", content)
    }

    #[test]
    fn test_metadata() {
        assert!(parse_string("@format=0
@test=\"ok\"
        ").is_ok());
        assert!(parse_string("@format=0
@test=\"ok\"
@test=\"ko\"
        ").is_err());
        assert!(parse_string("@format=0
@comment=\"ok\"
## ko
        ").is_err());
    }

    #[test]
    fn test_enum() {
        assert!(parse_string("@format=0
enum abc { a, b, c }
        ").is_ok());
        assert!(parse_string("@format=0
## enum comment
enum abc { a, b, c }
        ").is_err());
    }

    #[test]
    fn test_resource() {
        assert!(parse_string("@format=0
## resource comment
resource File(name)
        ").is_ok());
        assert!(parse_string("@format=0
## resource comment
resource File(name)
resource File(name2)
        ").is_err());
        assert!(parse_string("@format=0
resource File(name)
File state content() { }
        ").is_ok());
        assert!(parse_string("@format=0
resource File(name)
File2 state content() { }
        ").is_err());
        assert!(parse_string("@format=0
resource File(name)
File state content() { }
File state content() { }
        ").is_ok()); // no duplicate check here
    }

    #[test]
    fn test_defaults() {
        assert!(parse_string("@format=0
resource File(name=\"default\")
        ").is_ok());
        assert!(parse_string("@format=0
resource File(name=\"default\",path)
        ").is_ok()); // no default order check here
        assert!(parse_string("@format=0
resource File(name=\"defaul${\")
        ").is_err());
    }
}