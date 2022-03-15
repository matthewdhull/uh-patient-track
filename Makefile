all: test preprocess_uh_aot preprocess_noms

test: 
	echo "you haven't created any tests yet!"

preprocess_uh_aot:
	Rscript "scripts/preprocess_uh_aot.R"
	
preprocess_noms:
	Rscript "scripts/preprocess_noms.R"
