import java.math.BigInteger

class Day19 : Day {
    private fun parseInput(input: List<String>): Pair<Map<String, Workflow>, List<Map<PartCategory, Long>>> {
        val (workflowLines, partLines) = input.split("")

        val workflows = workflowLines.associate { line ->
            val name = line.substringBefore('{')
            val rulesString = line.substringAfter('{').substringBefore('}')
            val rules = rulesString.split(',').map {
                if (!it.contains(":")) return@map WorkflowRule(null, Destination.parse(it))

                val (conditionString, destination) = it.split(':')
                val category = conditionString.first().toPartCategory()
                val comparisonOperator = conditionString[1].toComparisonOperator()
                val comparedTo = conditionString.substring(2).toInt()

                WorkflowRule(
                    RuleCondition(category, comparisonOperator, comparedTo),
                    Destination.parse(destination)
                )
            }
            name to Workflow(name, rules)
        }

        val parts = partLines.map { line ->
            val partCategoryRatings = line.substringAfter('{').substringBefore('}').split(',')
            partCategoryRatings.associate { categoryRatings ->
                val (partCategory, rating) = categoryRatings.split("=")
                partCategory[0].toPartCategory() to rating.toLong()
            }
        }

        return workflows to parts
    }

    override fun part1(input: List<String>): String {
        val (workflows, parts) = parseInput(input)
        val startWorkflow = workflows["in"]!!
        val acceptedParts = parts.filter { part -> part.sendThrough(startWorkflow, workflows) == Accepted }
        return acceptedParts.sumOf { part -> part.values.sum() }.toString()
    }

    private fun findVariations(
        curWorkflow: Workflow,
        partRanges: Map<PartCategory, IntRange>,
        workflows: Map<String, Workflow>
    ): BigInteger {
        val curPart = partRanges.toMutableMap()
        return curWorkflow.rules.map { rule ->
            var nextPart = curPart
            if(rule.condition != null) {
                val curCategoryRange = curPart[rule.condition.category]!!

                if(rule.condition.comparedTo !in curCategoryRange) {
                    return@map BigInteger.ZERO
                }

                if (rule.condition.comparisonOperator == ComparisonOperator.LessThan) {
                    nextPart = curPart.toMutableMap().also {
                        it[rule.condition.category] = curCategoryRange.first..<rule.condition.comparedTo
                    }
                    curPart[rule.condition.category] = rule.condition.comparedTo..curCategoryRange.last
                } else {
                    nextPart = curPart.toMutableMap().also {
                        it[rule.condition.category] = (rule.condition.comparedTo + 1)..curCategoryRange.last
                    }
                    curPart[rule.condition.category] = curCategoryRange.first..rule.condition.comparedTo
                }
            }

            when (rule.destination) {
                is Rejected -> BigInteger.ZERO
                is Accepted -> nextPart.values.map { it.count().toBigInteger() }.reduce(BigInteger::times)
                is Forward -> findVariations(workflows[rule.destination.forwardTo]!!, nextPart, workflows)
            }
        }.reduce(BigInteger::plus)
    }

    override fun part2(input: List<String>): String {
        val (workflows) = parseInput(input)
        val startWorkflow = workflows["in"]!!
        val startPart = PartCategory.values().associateWith { 1..4000 }
        return findVariations(startWorkflow, startPart, workflows).toString()
    }
}

private data class Workflow(val name: String, val rules: List<WorkflowRule>)

private data class WorkflowRule(val condition: RuleCondition?, val destination: Destination) {
    fun evaluate(part: Map<PartCategory, Long>) : Boolean {
        if(condition == null) {
            return true
        }

        val rating = part[condition.category]!!
        return when (condition.comparisonOperator) {
            ComparisonOperator.LessThan -> rating < condition.comparedTo
            ComparisonOperator.GreaterThan -> rating > condition.comparedTo
        }
    }
}

private enum class PartCategory {
    ExtremelyCoolLooking,
    Musical,
    Aerodynamic,
    Shiny;
}

private fun Map<PartCategory, Long>.sendThrough(workflow: Workflow, allWorkflows: Map<String, Workflow>): Destination {
    val res = workflow.rules.first { it.evaluate(this) }

    if(res == null) {
        throw Error("No rules evaluated to true for part $this in workflow ${workflow.name}")
    }

    return if(res.destination is Forward) {
        this.sendThrough(allWorkflows[res.destination.forwardTo]!!, allWorkflows)
    } else {
        res.destination
    }
}

private sealed class Destination {
    companion object {
        fun parse(string: String): Destination {
            return when(string) {
                "A" -> Accepted
                "R" -> Rejected
                else -> Forward(string)
            }
        }
    }
}
private data object Accepted : Destination()
private data object Rejected : Destination()
private data class Forward(val forwardTo: String)  : Destination()

private fun Char.toPartCategory(): PartCategory {
    return when (this) {
        'x' -> PartCategory.ExtremelyCoolLooking
        'm' -> PartCategory.Musical
        'a' -> PartCategory.Aerodynamic
        's' -> PartCategory.Shiny
        else -> throw Error("Can't convert $this to PartCategory")
    }
}


private data class RuleCondition(
    val category: PartCategory,
    val comparisonOperator: ComparisonOperator,
    val comparedTo: Int
)

private enum class ComparisonOperator {
    GreaterThan, LessThan;
}

private fun Char.toComparisonOperator(): ComparisonOperator {
    return when (this) {
        '>' -> ComparisonOperator.GreaterThan
        '<' -> ComparisonOperator.LessThan
        else -> throw Error("Can't convert $this to ComparisonOperator")
    }
}
