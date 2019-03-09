val applyIfEquivalent : Lang.rule -> Lang.judgement list -> Lang.judgement option
val negateStatement : Lang.statement -> Lang.statement
val contradicts : Lang.statement -> Lang.statement -> bool
val negateQuality : Lang.quality -> Lang.quality
val negateQuantity : Lang.quantity -> Lang.quantity
