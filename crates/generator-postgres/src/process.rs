use schema::{process::NamingConvention, root::SchemaProcessOptions};

pub fn default_options() -> SchemaProcessOptions {
	SchemaProcessOptions {
		generator_supports_domain: true,
		naming_convention: NamingConvention::Postgres,
	}
}
