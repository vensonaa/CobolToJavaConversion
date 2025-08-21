#!/usr/bin/env python3
"""
Command Line Interface for COBOL to Java Conversion
"""

import asyncio
import argparse
import sys
import os
from pathlib import Path

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from main import convert_cobol_to_java
from utils.file_utils import load_cobol_file, save_conversion_results, create_conversion_report

def parse_arguments():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Convert COBOL code to Java using LangGraph multi-agent system",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Convert COBOL file to Java
  python cli.py --input sample.cbl --output HelloWorld.java
  
  # Convert with prior knowledge
  python cli.py --input payroll.cbl --knowledge "Banking application with file I/O"
  
  # Convert and save all results
  python cli.py --input complex.cbl --save-all --output-dir results/
  
  # Interactive mode
  python cli.py --interactive
        """
    )
    
    # Input options
    input_group = parser.add_mutually_exclusive_group(required=True)
    input_group.add_argument(
        "--input", "-i",
        type=str,
        help="Path to COBOL source file"
    )
    input_group.add_argument(
        "--interactive", "-I",
        action="store_true",
        help="Run in interactive mode to input COBOL code"
    )
    
    # Output options
    parser.add_argument(
        "--output", "-o",
        type=str,
        help="Path for output Java file (optional)"
    )
    parser.add_argument(
        "--output-dir", "-d",
        type=str,
        default="output",
        help="Directory for output files (default: output)"
    )
    parser.add_argument(
        "--save-all", "-s",
        action="store_true",
        help="Save all conversion artifacts (Java, pseudo code, summary)"
    )
    
    # Conversion options
    parser.add_argument(
        "--knowledge", "-k",
        type=str,
        help="Prior knowledge or requirements for conversion"
    )
    parser.add_argument(
        "--max-iterations", "-m",
        type=int,
        default=5,
        help="Maximum review-fix iterations (default: 5)"
    )
    
    # Display options
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Verbose output with detailed progress"
    )
    parser.add_argument(
        "--quiet", "-q",
        action="store_true",
        help="Suppress progress output"
    )
    parser.add_argument(
        "--show-pseudo",
        action="store_true",
        help="Display pseudo code in output"
    )
    parser.add_argument(
        "--show-summary",
        action="store_true",
        help="Display conversion summary in output"
    )
    
    return parser.parse_args()

def get_cobol_code_interactive():
    """Get COBOL code interactively from user."""
    print("Enter your COBOL code (press Ctrl+D or Ctrl+Z when finished):")
    print("-" * 50)
    
    lines = []
    try:
        while True:
            line = input()
            lines.append(line)
    except (EOFError, KeyboardInterrupt):
        pass
    
    return "\n".join(lines)

def display_progress(message: str, verbose: bool = False):
    """Display progress message if verbose mode is enabled."""
    if verbose:
        print(f"🔄 {message}")

def display_results(result: dict, args):
    """Display conversion results based on arguments."""
    print("\n" + "=" * 60)
    print("🎯 CONVERSION RESULTS")
    print("=" * 60)
    
    # Basic info
    print(f"✅ Status: {result['status']}")
    print(f"🔄 Review Iterations: {result['iterations']}")
    print(f"📝 Original COBOL Size: {len(result['original_cobol'])} characters")
    print(f"☕ Final Java Size: {len(result['final_java_code'])} characters")
    
    # Show pseudo code if requested
    if args.show_pseudo:
        print(f"\n📋 PSEUDO CODE ANALYSIS:")
        print("-" * 40)
        print(result['pseudo_code'])
    
    # Show Java code
    print(f"\n☕ FINAL JAVA CODE:")
    print("-" * 40)
    print(result['final_java_code'])
    
    # Show review comments if any
    if result['review_comments']:
        print(f"\n⚠️  REVIEW COMMENTS:")
        print("-" * 40)
        for i, comment in enumerate(result['review_comments'], 1):
            print(f"{i}. {comment}")
    
    # Show summary if requested
    if args.show_summary:
        print(f"\n📝 CONVERSION SUMMARY:")
        print("-" * 40)
        print(result['summary'])

async def main():
    """Main CLI function."""
    args = parse_arguments()
    
    # Validate arguments
    if args.input and not os.path.exists(args.input):
        print(f"❌ Error: Input file '{args.input}' not found.")
        sys.exit(1)
    
    if args.quiet and args.verbose:
        print("❌ Error: Cannot use both --quiet and --verbose options.")
        sys.exit(1)
    
    try:
        # Get COBOL code
        if args.interactive:
            display_progress("Getting COBOL code interactively...", args.verbose)
            cobol_code = get_cobol_code_interactive()
            if not cobol_code.strip():
                print("❌ Error: No COBOL code provided.")
                sys.exit(1)
        else:
            display_progress(f"Reading COBOL file: {args.input}", args.verbose)
            cobol_code = load_cobol_file(args.input)
        
        # Get prior knowledge
        prior_knowledge = args.knowledge or ""
        
        # Run conversion
        if not args.quiet:
            print("🚀 Starting COBOL to Java conversion...")
            print(f"📝 COBOL Code Length: {len(cobol_code)} characters")
            if prior_knowledge:
                print(f"📚 Prior Knowledge: {prior_knowledge[:100]}...")
        
        display_progress("Initializing conversion workflow...", args.verbose)
        result = await convert_cobol_to_java(cobol_code, prior_knowledge)
        
        if not args.quiet:
            print("✅ Conversion completed!")
        
        # Display results
        if not args.quiet:
            display_results(result, args)
        
        # Save results
        if args.save_all or args.output:
            display_progress("Saving conversion results...", args.verbose)
            
            # Determine base filename
            if args.output:
                base_filename = Path(args.output).stem
            elif args.input:
                base_filename = Path(args.input).stem
            else:
                base_filename = "interactive_conversion"
            
            # Save all results
            file_paths = save_conversion_results(
                result, 
                base_filename, 
                args.output_dir
            )
            
            # Save specific Java file if requested
            if args.output:
                java_path = Path(args.output)
                java_path.parent.mkdir(parents=True, exist_ok=True)
                with open(java_path, 'w', encoding='utf-8') as f:
                    f.write(result['final_java_code'])
                file_paths['java_specific'] = str(java_path)
            
            if not args.quiet:
                print(f"\n💾 Results saved to:")
                for file_type, file_path in file_paths.items():
                    print(f"   {file_type.upper()}: {file_path}")
                
                # Create and save report
                report = create_conversion_report(result, file_paths)
                report_path = os.path.join(args.output_dir, f"{base_filename}_report.txt")
                with open(report_path, 'w', encoding='utf-8') as f:
                    f.write(report)
                print(f"   REPORT: {report_path}")
        
        # Exit with appropriate code
        if result['status'] == 'conversion_complete':
            sys.exit(0)
        else:
            print(f"⚠️  Warning: Conversion completed with status: {result['status']}")
            sys.exit(1)
            
    except KeyboardInterrupt:
        print("\n❌ Conversion interrupted by user.")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Error during conversion: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    asyncio.run(main())
