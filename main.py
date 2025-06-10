import argparse

from interpreter import *

def main():
    parser = argparse.ArgumentParser(
        description="A concatenative language interpreter.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("filepath", help="Path to the source file to execute.")
    parser.add_argument("-d", "--debug", action="store_true", help="Print the final stack state after execution.")
    args = parser.parse_args()

    try:
        with open(args.filepath, 'r',encoding='utf-8') as f:
            code = f.read()
    except FileNotFoundError:
        print(f"Error: File not found at '{args.filepath}'", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: Could not read file '{args.filepath}': {e}", file=sys.stderr)
        sys.exit(1)

    # --- New, Simpler Workflow ---
    # 1. Create an interpreter with the source code
    interpreter = Interpreter(code)
    
    # 2. Run it. The interpreter handles all parsing and execution internally.
    final_stack = interpreter.run()

    if args.debug:
        print("\n--- Execution Finished ---")
        print(f"Final stack state: {final_stack}")

if __name__ == "__main__":
    main()
