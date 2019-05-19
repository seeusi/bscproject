# bscproject (debugging)

- Seperated event handling from rendering for code reuse modularity.
The server function now handles what should happen when a file is uploaded, an 
option is picked, etc. How it's done is moved to function declared at the 
beggining of the script.

- Removed weird picker behaviour (there were picker resets in the previous 
event handling)
