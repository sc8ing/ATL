val applyIfEquivalent : Lang.rule -> Lang.judgement list -> Lang.judgement option
val negateStatement : Lang.statement -> Lang.statement
val contradicts : Lang.statement -> Lang.statement -> bool
