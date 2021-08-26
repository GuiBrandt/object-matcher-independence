package io.github.guibrandt.matchers

import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonPrimitive

sealed class JsonMatcher {
    abstract fun match(obj: JsonObject): Boolean
}

data class AttributeMatcher(val attribute: String, val values: Set<String>): JsonMatcher() {
    // TODO: deep attributes
    override fun match(obj: JsonObject) = obj[attribute]?.jsonPrimitive.toString() in values
}

data class ConjunctionMatcher(val terms: List<JsonMatcher>): JsonMatcher() {
    constructor(vararg terms: JsonMatcher): this(terms.toList())

    override fun match(obj: JsonObject) = terms.all { it.match(obj) }
}

data class NegativeMatcher(val term: JsonMatcher): JsonMatcher() {
    override fun match(obj: JsonObject) = !term.match(obj)
}
