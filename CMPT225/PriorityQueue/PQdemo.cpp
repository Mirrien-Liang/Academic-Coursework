#include "PQ.h"
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
using namespace std;

// Test program
int main() {

  // Test set 1
  // Test the following items:
  // 1. constructor with given int sets
  // 2. update priority
  // 3. extract heap min
  // 4. get size
  // 5. get debugging info using print_all()
  cout << "===============Test Case 1===============" << endl;
  vector<int> tasks= {1,2,3,4,5,6,7};
  vector<int> priorities= {1,2,3,4,5,6,7};
  
  // Testing Constructor
  cout << "\n[ Testing Constructor... ]" << endl;
  cout << "[ Inputs: ]\n  tasks = {1,2,3,4,5,6,7}\n  priorities = {1,2,3,4,5,6,7}" << endl;
  PQ<int> pq1 (tasks,priorities);
  pq1.print_all();
  
  // Testing update priority
  cout << "\n[ Testing updatePriority()... ]" << endl;
  // Test on existing task
  cout << "Try changing task 1's priority from 1 to 10..." << endl;
  pq1.updatePriority(1, 10);
  // Test on non-existing task
  cout << "Try changing non-existing task 10's priority to 20..." << endl;
  pq1.updatePriority(10,20);
  pq1.print_all();
  
  // Testing extract minimal
  cout << "\n[ Testing deleteMin()... ]" << endl; 
  for (long unsigned int i = 0; i < 8; i++) {
    cout << "[ Deleting Task " << pq1.deleteMin() << "... now the size is " << pq1.size() << " ]" << endl;
    pq1.print_all();
  }
    

  

  // Test set 2
  // Test the following items:
  // 1. empty constructor
  // 2. explicitly test isEmpty()
  // 3. insertion
  // 4. make empty
  // 5. get debugging info using print_all()

  cout << "\n===============Test Case 2===============" << endl;

  cout << "\n[ Testing empty constructor... ]" << endl;
  PQ<int> pq2;

  // Testing isEmpty()
  if (pq2.isEmpty()) cout << "Empty Constructor succeeded." << endl;
  else cout << "Empty constructor failed." << endl;

  // Testing Insertion in descending order (intrinsically more complex)
  cout << "\n[ Testing insertion... ]" << endl;
  for (int i = 10; i >= 1; i--) {
    pq2.insert(2*i,i);
    cout << "Inserting (" << 2*i << "," << i << ")..." << endl;
  }
  pq2.print_all();

  // Testing make empty
  cout << "\n[ Testing make empty... ]" << endl;
  pq2.makeEmpty();
  pq2.print_all();
  
    

  // Test set 3
  // Test the following items:
  // 1. float type
  // 2. non-empty constructor
  // 3. insertion
  // 4. find and delete min
  // 5. update priority
  // 6. make empty
  cout << "\n===============Test Case 3===============" << endl;

  cout << "\n[ Testing Float Type PQ Constructor... ]" << endl;
  cout << "[ Inputs: ]\n  tasks = {1.1,2.2,3.3,4.4,5.5,6.6,7.7}\n  priorities = {6,2,9,7,1,5,3}" << endl;
  vector<float> tasks_f= {1.1,2.2,3.3,4.4,5.5,6.6,7.7};
  vector<int> priorities_f= {6,2,9,7,1,5,3};

  // Testing float type PQ + constructor
  PQ<float> pq3 (tasks_f,priorities_f);
  pq3.print_all();

  // Testing float type insertion
  cout << "\n[ Testing Float Insertion... ]" << endl;
  cout << "Inserting (13.3,8)..." << endl;
  cout << "Inserting (0.4,4)..." << endl;
  pq3.insert(13.3, 8);
  pq3.insert(0.4, 4);
  pq3.print_all();

  // Testing find and delete min
  cout << "\n[ Testing Float find/delete min... ]" << endl;
  cout << "The task with minimal priority is " << pq3.findMin() << endl;
  for (long unsigned int i = 0; i < tasks_f.size(); i++) {
    cout << "[ Deleting Task " << pq3.deleteMin() << "... now the size is " << pq3.size() << " ]" << endl;
  }
  pq3.print_all();

  // Testing update priority
  cout << "\n[ Testing Float updatePriority()... ]" << endl;
  // Test on existing task
  cout << "Try change task 1.1's priority from 6 to 66" << endl;
  pq3.updatePriority(1.1, 66);
  // Test on non-existing task
  cout << "Try change non-existing task 10.0's priority" << endl;
  pq3.updatePriority(10.0,100);
  pq3.print_all();

  // Testing makeEmpty()
  cout << "\n[ Testing Float makeEmpty()... ]" << endl;
  pq3.makeEmpty();
  pq3.print_all();

  cout << "\n\nEnd All Test..." << endl;
  return 0;
}