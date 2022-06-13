# HaskellChat
The aim of the project was to implement the concepts of concurrency in Haskell by developing a stack app that spawns between ten users thready and simulates 100 messages being sent between those users at random selection and time interval.
#How to Compile and run code:
To run this code, unzip the file submitted and open the project folder in VS Code with latest Haskell support installed, the navigate into the folder and execute “stack build” into the terminal which will download and install the packages and libraries listed in the dependencies and compile the app. Next guaranteed no error on compiling the app, type the command “stack run” into the terminal and wait for the app to finish the simulation and print the desired output that also lets you interact with it.
#Design Choices Justification:
A new data type was created for each user containing a unique integer username between 1 and 10, a count of the sent and received messages, as well as an array of a new data type messaged that holds both incoming and outgoing messages in “inbox” and “outbox” respectively. Each message sent contains details regarding the username of the source and destination user. It also keeps track of the timestamp of when the message was sent in addition to the thread id of the sending user. To simulate real-world sentences are randomly generated from a text file “chat.txt” and sent as message between users.
The app makes use of three MVar to assist the implementation, putMVar which fills an MVar if it is empty and blocks otherwise, and takeMVar which empties an MVar if it is full and blocks otherwise. They are total, set to 0 initially keeps track of the sum of all the messages sent and gets incremented with every message sent until it reaches 100. “choosen” is the second MVar that hold the running at the moment, and it is updated with each thread run, lastly an array if the ten users are stored into an Mvar named “users” to make it easier to pick and update each user’s details after sending and receiving a message.
At the start of the program ten users are initialized with empty inbox/outbox and zero count and they are passed as input to a function that generated a thread for each user that in turn calls another function that takes a random delay between 5 and 15 milliseconds and sends a message to another receiver user different from the sender and choosen randomly and the cycle continues for each user until the total count of sent message reaches 100.
#Challenges faced:
A bit of revision and learning curve on Haskell data type was still needed especially with type conversion, likewise some functions would print unintended output, and some tend to get stuck the thread in a loop indefinitely while blocking the whole program, understanding the order of when to empty and fill an Mvar solved this at later stage of the project.
#Extra Functionality beyond Specifications:
With each running thread the message sent between the users is saved into an SQLite database “messages.sqlite” that keeps the order of the messages sent including the source, destination, content, and timestamp as well as the thread ID of the thread generating that message. This was also saved into local txt file “users.txt” and “messages.txt” that also keeps track of the. After finishing simulating 100 messages the app gives an option to print out the inbox and outbox folder along with the relevant metrics like respective count and timestamp of each of a choosen user. Inputting a integer in the range 1 to 10(inclusive) prints the content of each User, Input validation and error handling is performed when an empty or non-numeric input is given the app and it prompts an error and as you to enter again and the Program exits for any input out of the range between 1 and 10.
