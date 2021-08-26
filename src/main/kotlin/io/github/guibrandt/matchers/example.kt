package io.github.guibrandt.matchers

import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory

fun main() {
    val matcher1 = AttributeMatcher("attribute1", setOf("value1", "value2"))
    val matcher2 = ConjunctionMatcher(
        AttributeMatcher("attribute2", setOf("value2")),
        AttributeMatcher("attribute1", setOf("value3")),
    )

    val cnf = matcherToCNF(ConjunctionMatcher(matcher1, matcher2))
    println(cnf)

    val variables = mutableMapOf<AttributeAssignment, Int>()
    for (term in cnf.clauses.flatMap { it.terms }) {
        if (term.variable in variables) continue
        variables[term.variable] = variables.size + 1
    }

    val solver = SolverFactory.newLight()
    solver.timeout = 2
    solver.setExpectedNumberOfClauses(cnf.clauses.size)

    for (clause in cnf.clauses) {
        val sat4jClause = clause.terms
            .map { term ->
                val sign = if (term.negative) -1 else 1
                sign * variables[term.variable]!!
            }
            .toIntArray()

        solver.addClause(VecInt(sat4jClause))
    }

    println(solver.isSatisfiable)
}
