package com.github.kright.formulaparser

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

/**
	* Created by lgor on 3/31/17.
	*/
object FormulaParser extends RegexParsers with PackratParsers {

	def id: Parser[Id] = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ Id

	def number: Parser[Number] = "-" ~> number ^^ (n => Number(-n.value)) |
		("[0-9]+\\.[0-9]*".r | "[0-9]+".r) ^^ (s => Number(s.toDouble))

	def funcCall: Parser[FuncCall] = id ~ ("(" ~> expression <~ ")") ^^ {case id ~ exp => FuncCall(id, exp)}

	def value: Parser[Expression] = number | funcCall | id | ("(" ~> expression <~ ")")

	lazy val term: PackratParser[Expression] = term ~ ("*" | "/") ~ value ^^ binOperation | value

	lazy val expression: PackratParser[Expression] = expression ~ ("+" | "-") ~ term ^^ binOperation | term

	private def binOperation(p: Expression ~ String ~ Expression) = p match {
		case e1 ~ op ~ e2 => BinOperation(e1, BinOperator(op), e2)
	}

	def apply(code: String): Either[ParserError, Expression] =
		parse(expression, new PackratReader(new CharSequenceReader(code))) match {
			case Success(result, next) => Right(result)
			case NoSuccess(msg, next) => Left(ParserError(msg))
		}

	case class ParserError(msg: String)
}

sealed trait Expression

case class BinOperator(operator: String)

case class Number(value: Double) extends Expression
case class Id(name: String) extends Expression
case class BinOperation(left: Expression, op: BinOperator, right: Expression) extends Expression
case class FuncCall(funcName: Id, argument: Expression) extends Expression
