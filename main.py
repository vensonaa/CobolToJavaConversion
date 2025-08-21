#!/usr/bin/env python3
"""
COBOL to Java Conversion using LangGraph
A multi-agent system that converts COBOL code to Java with review and refinement.
"""

import os
import asyncio
from typing import Dict, List, Any, TypedDict
from dotenv import load_dotenv
from langgraph.graph import StateGraph, END
from langchain_groq import ChatGroq
from langchain_core.messages import HumanMessage

# Load environment variables
load_dotenv()

# State definition
class ConversionState(TypedDict):
    """State for the COBOL to Java conversion workflow."""
    cobol_code: str
    prior_knowledge: str
    pseudo_code: str
    java_code: str
    review_comments: List[str]
    fixed_java_code: str
    summary: str
    current_step: str
    iteration_count: int
    max_iterations: int

# Initialize the LLM
llm = ChatGroq(
    model_name="llama3-70b-8192",
    temperature=0.1,
    api_key=os.getenv("GROQ_API_KEY")
)

# Agent definitions
class PlannerAgent:
    """Planner agent that creates a conversion plan based on prior knowledge."""
    
    def __init__(self):
        self.llm = llm
    
    def plan_conversion(self, state: ConversionState) -> ConversionState:
        """Create a conversion plan based on COBOL code and prior knowledge."""
        prompt = f"""
        You are a COBOL to Java conversion planner. Based on the provided COBOL code and prior knowledge, 
        create a detailed conversion plan.
        
        COBOL Code:
        {state['cobol_code']}
        
        Prior Knowledge:
        {state['prior_knowledge']}
        
        Create a comprehensive plan that includes:
        1. Analysis of the COBOL structure
        2. Key conversion challenges
        3. Required Java patterns and libraries
        4. Step-by-step conversion approach
        
        Return your plan as a structured response.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        # Update state with plan
        state['summary'] = f"Conversion Plan Created:\n{response.content}"
        state['current_step'] = 'planning_complete'
        
        return state

class ExecutorAgent:
    """Executor agent that orchestrates the conversion process."""
    
    def __init__(self):
        self.llm = llm
    
    def execute_conversion(self, state: ConversionState) -> ConversionState:
        """Execute the conversion plan by delegating to specialized agents."""
        prompt = f"""
        You are the executor agent for COBOL to Java conversion. 
        The planning phase is complete. Now you need to coordinate the conversion process.
        
        Current State:
        - COBOL Code: {state['cobol_code'][:200]}...
        - Plan: {state['summary']}
        
        Proceed with the conversion workflow:
        1. Delegate to Pseudo Code Generator
        2. Delegate to Java Code Generator
        3. Delegate to Code Reviewer
        4. If issues found, delegate to Code Fixer
        5. Repeat review-fix cycle until clean
        6. Provide final summary
        
        Update the state to proceed to the next step.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        state['current_step'] = 'execution_started'
        state['summary'] += f"\n\nExecution Started:\n{response.content}"
        
        return state

class PseudoCodeGeneratorAgent:
    """Agent that understands COBOL and generates pseudo code."""
    
    def __init__(self):
        self.llm = llm
    
    def generate_pseudo_code(self, state: ConversionState) -> ConversionState:
        """Generate pseudo code from COBOL code."""
        prompt = f"""
        You are an expert COBOL analyst. Analyze the following COBOL code and generate 
        clear, detailed pseudo code that explains the logic and structure.
        
        COBOL Code:
        {state['cobol_code']}
        
        Generate pseudo code that:
        1. Explains the main program logic
        2. Identifies data structures and their purposes
        3. Describes control flow and business rules
        4. Highlights any special COBOL constructs that need special handling in Java
        
        Make the pseudo code clear and comprehensive for Java developers to understand.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        state['pseudo_code'] = response.content
        state['current_step'] = 'pseudo_code_generated'
        state['summary'] += f"\n\nPseudo Code Generated:\n{response.content[:200]}..."
        
        return state

class JavaCodeGeneratorAgent:
    """Agent that generates Java code from COBOL code."""
    
    def __init__(self):
        self.llm = llm
    
    def generate_java_code(self, state: ConversionState) -> ConversionState:
        """Generate Java code from COBOL code and pseudo code."""
        prompt = f"""
        You are an expert Java developer specializing in COBOL to Java conversion.
        
        COBOL Code:
        {state['cobol_code']}
        
        Pseudo Code:
        {state['pseudo_code']}
        
        Generate high-quality Java code that:
        1. Maintains the original business logic
        2. Uses modern Java patterns and best practices
        3. Handles COBOL-specific constructs appropriately
        4. Includes proper error handling
        5. Uses meaningful variable and method names
        6. Includes comments explaining complex logic
        
        Return only the Java code with appropriate imports and class structure.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        state['java_code'] = response.content
        state['current_step'] = 'java_code_generated'
        state['summary'] += f"\n\nJava Code Generated:\n{response.content[:200]}..."
        
        return state

class CodeReviewerAgent:
    """Agent that reviews Java code and provides feedback."""
    
    def __init__(self):
        self.llm = llm
    
    def review_code(self, state: ConversionState) -> ConversionState:
        """Review the generated Java code and provide feedback."""
        java_code = state.get('fixed_java_code', state['java_code'])
        
        prompt = f"""
        You are a senior Java code reviewer. Review the following Java code that was 
        converted from COBOL and provide detailed feedback.
        
        Original COBOL Code:
        {state['cobol_code']}
        
        Generated Java Code:
        {java_code}
        
        Review the code for:
        1. Correctness of business logic conversion
        2. Java best practices and conventions
        3. Code quality and readability
        4. Error handling and edge cases
        5. Performance considerations
        6. Security issues
        
        Provide specific, actionable feedback. If the code is acceptable, return "NO_ISSUES".
        Otherwise, list specific issues that need to be addressed.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        if "NO_ISSUES" in response.content.upper():
            state['review_comments'] = []
            state['current_step'] = 'review_passed'
        else:
            state['review_comments'] = [response.content]
            state['current_step'] = 'review_failed'
        
        state['summary'] += f"\n\nCode Review:\n{response.content[:200]}..."
        
        return state

class CodeFixerAgent:
    """Agent that fixes issues identified by the code reviewer."""
    
    def __init__(self):
        self.llm = llm
    
    def fix_code(self, state: ConversionState) -> ConversionState:
        """Fix the Java code based on review comments."""
        java_code = state.get('fixed_java_code', state['java_code'])
        
        prompt = f"""
        You are a Java code fixer. Fix the following Java code based on the review comments.
        
        Original COBOL Code:
        {state['cobol_code']}
        
        Current Java Code:
        {java_code}
        
        Review Comments:
        {state['review_comments']}
        
        Fix all the issues mentioned in the review comments. Ensure the code:
        1. Addresses all specific issues mentioned
        2. Maintains the original business logic
        3. Follows Java best practices
        4. Is well-documented and readable
        
        Return the corrected Java code.
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        state['fixed_java_code'] = response.content
        state['current_step'] = 'code_fixed'
        state['iteration_count'] += 1
        state['summary'] += f"\n\nCode Fixed (Iteration {state['iteration_count']}):\n{response.content[:200]}..."
        
        return state

class FinalSummarizerAgent:
    """Agent that provides final summary of the conversion process."""
    
    def __init__(self):
        self.llm = llm
    
    def summarize_conversion(self, state: ConversionState) -> ConversionState:
        """Provide a comprehensive summary of the conversion process."""
        prompt = f"""
        You are a conversion process summarizer. Provide a comprehensive summary of the 
        COBOL to Java conversion that was just completed.
        
        Conversion Details:
        - Original COBOL Code Length: {len(state['cobol_code'])} characters
        - Final Java Code Length: {len(state.get('fixed_java_code', state['java_code']))} characters
        - Number of Review Iterations: {state['iteration_count']}
        - Review Comments: {state['review_comments']}
        
        Provide a summary that includes:
        1. Overview of the conversion process
        2. Key challenges encountered and how they were resolved
        3. Quality of the final Java code
        4. Recommendations for further improvements
        5. Lessons learned for future conversions
        """
        
        response = self.llm.invoke([HumanMessage(content=prompt)])
        
        state['summary'] = response.content
        state['current_step'] = 'conversion_complete'
        
        return state

# Decision functions
def should_continue_review(state: ConversionState) -> str:
    """Decide whether to continue the review-fix cycle."""
    if state['current_step'] == 'review_passed':
        return "summarize"
    elif state['iteration_count'] >= state['max_iterations']:
        return "summarize"
    else:
        return "fix_code"

def should_fix_code(state: ConversionState) -> str:
    """Decide whether to fix code or continue review."""
    if state['current_step'] == 'review_failed':
        return "fix_code"
    else:
        return "review_code"

# Create the workflow graph
def create_conversion_graph():
    """Create the LangGraph workflow for COBOL to Java conversion."""
    
    # Initialize agents
    planner = PlannerAgent()
    executor = ExecutorAgent()
    pseudo_generator = PseudoCodeGeneratorAgent()
    java_generator = JavaCodeGeneratorAgent()
    reviewer = CodeReviewerAgent()
    fixer = CodeFixerAgent()
    summarizer = FinalSummarizerAgent()
    
    # Create the graph
    workflow = StateGraph(ConversionState)
    
    # Add nodes
    workflow.add_node("planner", planner.plan_conversion)
    workflow.add_node("executor", executor.execute_conversion)
    workflow.add_node("pseudo_generator", pseudo_generator.generate_pseudo_code)
    workflow.add_node("java_generator", java_generator.generate_java_code)
    workflow.add_node("reviewer", reviewer.review_code)
    workflow.add_node("fixer", fixer.fix_code)
    workflow.add_node("summarizer", summarizer.summarize_conversion)
    
    # Add edges - START with planner
    workflow.set_entry_point("planner")
    workflow.add_edge("planner", "executor")
    workflow.add_edge("executor", "pseudo_generator")
    workflow.add_edge("pseudo_generator", "java_generator")
    workflow.add_edge("java_generator", "reviewer")
    
    # Add conditional edges for review-fix cycle
    workflow.add_conditional_edges(
        "reviewer",
        should_continue_review,
        {
            "summarize": "summarizer",
            "fix_code": "fixer"
        }
    )
    
    workflow.add_conditional_edges(
        "fixer",
        should_fix_code,
        {
            "review_code": "reviewer",
            "fix_code": "fixer"
        }
    )
    
    workflow.add_edge("summarizer", END)
    
    return workflow.compile()

# Main execution function
async def convert_cobol_to_java(cobol_code: str, prior_knowledge: str = "") -> Dict[str, Any]:
    """
    Convert COBOL code to Java using the multi-agent LangGraph workflow.
    
    Args:
        cobol_code: The COBOL source code to convert
        prior_knowledge: Additional knowledge or requirements for the conversion
    
    Returns:
        Dictionary containing the conversion results
    """
    
    # Initialize state
    initial_state = ConversionState(
        cobol_code=cobol_code,
        prior_knowledge=prior_knowledge,
        pseudo_code="",
        java_code="",
        review_comments=[],
        fixed_java_code="",
        summary="",
        current_step="started",
        iteration_count=0,
        max_iterations=5
    )
    
    # Create and run the graph
    graph = create_conversion_graph()
    
    print("🚀 Starting COBOL to Java conversion...")
    print(f"📝 COBOL Code Length: {len(cobol_code)} characters")
    
    result = await graph.ainvoke(initial_state)
    
    print("✅ Conversion completed!")
    
    return {
        "original_cobol": result["cobol_code"],
        "pseudo_code": result["pseudo_code"],
        "final_java_code": result.get("fixed_java_code", result["java_code"]),
        "review_comments": result["review_comments"],
        "summary": result["summary"],
        "iterations": result["iteration_count"],
        "status": result["current_step"]
    }

# Example usage
if __name__ == "__main__":
    # Example COBOL code
    sample_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NAME PIC X(20) VALUE 'World'.
           01 WS-MESSAGE PIC X(50).
       
       PROCEDURE DIVISION.
           MOVE 'Hello, ' TO WS-MESSAGE
           STRING WS-MESSAGE DELIMITED BY SIZE
                  WS-NAME DELIMITED BY SPACE
                  '!' DELIMITED BY SIZE
                  INTO WS-MESSAGE
           DISPLAY WS-MESSAGE
           STOP RUN.
    """
    
    # Example prior knowledge
    prior_knowledge = """
    This is a simple COBOL program that displays a greeting message.
    The program uses STRING concatenation and DISPLAY for output.
    Convert to a Java program with similar functionality.
    """
    
    # Run the conversion
    async def main():
        result = await convert_cobol_to_java(sample_cobol, prior_knowledge)
        
        print("\n" + "="*50)
        print("CONVERSION RESULTS")
        print("="*50)
        print(f"Status: {result['status']}")
        print(f"Iterations: {result['iterations']}")
        print(f"\nPseudo Code:\n{result['pseudo_code']}")
        print(f"\nFinal Java Code:\n{result['final_java_code']}")
        print(f"\nSummary:\n{result['summary']}")
    
    asyncio.run(main())
