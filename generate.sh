stack exec uuagc -- "src/ByteCode.ag" --data --rename --strictdata --genlinepragmas

stack exec uuagc -- "src/PrettyCode.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/PrettyTree.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/TrfInjectAbc.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/ExtractAbc.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/Language.ag" --data --rename --genlinepragmas

stack exec uuagc -- "src/SymbolTables.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/CFG.ag" --data --rename --strictdata --genlinepragmas

stack exec uuagc -- "src/TrfToCFG.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/InstrSize.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/InstrLocFilter.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/SymView.ag" --data --rename --strictdata --genlinepragmas

stack exec uuagc -- "src/GenInstrLib.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas

stack exec uuagc -- "src/TrfInjectRefl.ag" --rename --catas --semfuns --signatures --strictcase --strictwrap --optimize --genlinepragmas
