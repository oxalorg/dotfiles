# Global Rules

Use `trash` to delete files and directories.

Learn how `trash` works by running the command `trash --help`

IMPORTANT: DON'T USE `rm` TO DELETE FILES AND DIRECTORIES, AS THAT IS PERMANENT AND CANNOT BE UNDONE. INSTEAD, USE `trash` TO MOVE FILES TO THE TRASH/RECYCLE BIN, FROM WHERE THEY CAN BE RESTORED IF NEEDED.


# Python package manager: "uv"

Instead of using `pip`, `pip3`, or `python` or `python3`, always use `uv`. 

```
uv init 	        # Initialise a project in the current directory  
uv add requests 	# Add requests as a dependency
uv add A B C 	    # Add A, B, and C as dependencies
uv add -r requirements.txt 	# Add dependencies from the file requirements.txt
uv add --dev pytest 	    # Add pytest as a development dependency
uv run pytest 	    # Run the pytest executable that is installed in your project
uv remove requests 	# Remove requests as a dependency
uv remove A B C 	# Remove A, B, C, and their transitive dependencies
uv tree 	        # See the  project dependencies tree
uv lock --upgrade 	# Upgrade the dependencies' versions
```

Working with pythons scripts:

```
uv init --script myscript.py 	            # Initialise the script myscript.py
uv init --script myscript.py --python 3.X 	# Initialise the script myscript.py and pin it to version 3.X
uv add click --script myscript.py 	        # Add the dependency click to the script
uv remove click --script myscript.py 	    # Remove the dependency click from the script
uv run myscript.py 	                        # Run the script myscript.py
uv run --python 3.X myscript.py 	        # Run the script with the given Python version
uv run --with click myscript.py 	        # Run the script along with the click dependency 
```
