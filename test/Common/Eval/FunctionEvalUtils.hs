module Common.Eval.FunctionEvalUtils where

nbArgEntry s n = [(s,replicate n mockArg,error "FunctionEvalTestUtils::nbArgEntry::func [Should not be called]")]
mockArg        = error "FunctionEvalTestUtils::mockArg [Should not be called]"
