package io.github.guibrandt.matchers

import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.reader.DimacsReader

data class CNF<T>(val clauses: List<Clause<T>>) {
    constructor(vararg clauses: Clause<T>) : this(clauses.toList())

    fun conjunction(other: CNF<T>): CNF<T> = CNF(clauses + other.clauses)

    fun disjunction(other: CNF<T>): CNF<T> {
        val newClauses: List<Clause<T>> = clauses.flatMap { clause1 ->
            clause1.terms.flatMap { term1 ->
                other.clauses.flatMap { clause2 ->
                    clause2.terms.map { term2 ->
                        Clause(term1, term2)
                    }
                }
            }
        }
        return CNF(newClauses)
    }

    override fun toString() = clauses.joinToString(" && ") { "($it)" }
}

data class Clause<T>(val terms: List<Term<T>>) {
    constructor(vararg terms: Term<T>) : this(terms.toList())

    override fun toString() = terms.joinToString(" || ", transform = Term<T>::toString)
}

data class Term<T>(val negative: Boolean, val variable: T) {
    override fun toString() = if (negative) "!($variable)" else "$variable"
}

data class AttributeAssignment(val attribute: String, val value: String) {
    override fun toString(): String = "$attribute == \"$value\""
}

fun matcherToCNF(matcher: JsonMatcher) =
    functionalDependencies(variables(matcher))
        .conjunction(convertToCNF(matcher))

private fun variables(matcher: JsonMatcher): Set<AttributeAssignment> = when (matcher) {
    is AttributeMatcher -> matcher.values.map { AttributeAssignment(matcher.attribute, it) }.toSet()
    is ConjunctionMatcher -> matcher.terms.fold(emptySet()) { acc, term -> acc + variables(term) }
    is NegativeMatcher -> variables(matcher.term)
}

private fun convertToCNF(matcher: JsonMatcher): CNF<AttributeAssignment> = when (matcher) {
    is AttributeMatcher -> {
        val terms = matcher.values.map { value ->
            val assignment = AttributeAssignment(matcher.attribute, value)
            Term(false, assignment)
        }
        CNF(Clause(terms))
    }

    is ConjunctionMatcher ->
        matcher.terms
            .map(::matcherToCNF)
            .reduce(CNF<AttributeAssignment>::conjunction)

    is NegativeMatcher -> when (val term = matcher.term) {
        is AttributeMatcher -> {
            if (term.values.size == 1) {
                val assignment = AttributeAssignment(term.attribute, term.values.first())
                CNF(Clause(Term(true, assignment)))
            } else {
                val terms = term.values.map { value ->
                    val attributeMatcher = AttributeMatcher(term.attribute, setOf(value))
                    NegativeMatcher(attributeMatcher)
                }
                matcherToCNF(ConjunctionMatcher(terms))
            }
        }

        is ConjunctionMatcher ->
            term.terms
                .map(::NegativeMatcher)
                .map(::matcherToCNF)
                .reduce(CNF<AttributeAssignment>::disjunction)

        is NegativeMatcher -> matcherToCNF(term.term)
    }
}

private fun functionalDependencies(variables: Set<AttributeAssignment>): CNF<AttributeAssignment> =
    variables
        .groupBy { it.attribute }
        .flatMap { (_, values) ->
            allPairs(values)
                .map { (value1, value2) ->
                    CNF(Clause(Term(true, value1), Term(true, value2)))
                }
        }
        .fold(CNF(emptyList()), CNF<AttributeAssignment>::conjunction)

private fun <T> allPairs(list: List<T>): List<Pair<T, T>> =
    list.flatMapIndexed { i, value -> (i + 1 until list.size).map { value to list[it] } }
