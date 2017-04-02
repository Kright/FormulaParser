package com.github.kright.formulaparser

/**
	* Created by lgor on 4/2/17.
	*/
object Main extends App {
	def eval(code: String,
	         variables: (String) => Double = Map.empty,
	         functions: (String) => (Double) => Double = Map.empty) = {
		val parsed = FormulaParser(code)
		parsed.left.foreach(error => println(s"\'$code\' parsing error: $error"))
		parsed.right.map(expr => Evaluator(expr, variables, functions)).foreach(d => println(s"\'$code\' = $d"))
	}

	eval("1")
	eval("0.1")
	eval("1.")
	eval("  1  ")
	eval("-0.1")

	eval("1+2")
	eval("2-1")
	eval("2*3")
	eval("4/2")

	val vars = Map(
		"pi" -> math.Pi,
		"e" -> math.E)

	val funcs: (String) => (Double) => Double = Map(
		"sin" -> math.sin,
		"cos" -> math.cos,
		"inc" -> { d: Double => d + 1 }
	)

	eval("pi", vars)
	eval("inc(e)", vars, funcs)

	eval("2+2*2")
	eval("1+2*(3+4*5)")
	eval("8/2/2")
	eval("8-1-2")

	eval("1. + 2.0 * sin(pi / 2)", vars, funcs)
}
