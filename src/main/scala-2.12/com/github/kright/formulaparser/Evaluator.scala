package com.github.kright.formulaparser

/**
	* Created by lgor on 4/2/17.
	*/
object Evaluator {
	def apply(expression: Expression,
	          variables: (String) => Double = Map.empty,
	          functions: (String) => (Double) => Double = Map.empty): Double = {

		def eval(exp: Expression) = this (exp, variables, functions)

		expression match {
			case Number(value) => value
			case Id(name) => variables(name)
			case BinOperation(left, op, right) => operator2func(op)(eval(left), eval(right))
			case FuncCall(funcId, expr) => functions(funcId.name)(eval(expr))
		}
	}

	def operator2func(binOperator: BinOperator): (Double, Double) => Double =
		binOperator.operator match {
			case "+" => (a, b) => a + b
			case "-" => (a, b) => a - b
			case "*" => (a, b) => a * b
			case "/" => (a, b) => a / b
		}
}
