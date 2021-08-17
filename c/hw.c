#define _GNU_SOURCE  // cause stdio.h to include vasprintf
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// make a doubly linked list of heap allocated strings, functions
// to add, remove, and print the list of strings

typedef struct node {
  char* str;
  struct node* next;
  struct node* prev;
} node;

typedef struct llist {
  struct node* head;
  struct node* tail;
} llist;

void iter(int(*action)(node*), llist* ll) {
  if (ll == NULL) return;
  if (ll->head == NULL) return;
  node* n = ll->head;
  int done = 0;
  while(n != NULL && done == 0) {
    done = action(n);
    n = n->next;
  }
}

// to support a find function, iteri accepts a done signal from
// the action. If the done signal is zero, the current index
// is returned. In all other cases, -1 is returned.
int iteri(int(*action)(int, node*), llist* ll) {
  if (ll == NULL) return -1;
  if (ll->head == NULL) return -1;
  node* n = ll->head;
  int i = 0;
  int done = 0;
  while(n != NULL) {
    done = action(i, n);
    if (done != 0) {
      // on done signal, return current index
      return i;
    }
    i++;
    n = n->next;
  }
  return -1;  // if we never got the done signal, return -1
}

int _pnode(node* n) {
  printf("%s ", n->str);
  return 0;
}

int _pnodei(int i, node* n) {
  printf("%i\t%s\n", i, n->str);
  return 0;
}

void printll(llist* ll) {
  iter(_pnode, ll);
  printf("\n");
}

void printill(llist* ll) {
  iteri(_pnodei, ll);
  printf("\n");
}

int isEmpty(llist* ll) {
  return ll->head == NULL && ll->tail == NULL;
}

node* mkNode(char* s) {
  node* n = malloc(sizeof(node));
  char* ns;
  asprintf(&ns, "%s", s);
  n->str = ns;
  n->next = NULL;
  n->prev = NULL;
  return n;
}

void append(llist* ll, char* s) {
  printf("Appending %s\n", s);
  if (ll == NULL) return;
  node* n = mkNode(s);
  if (isEmpty(ll)) {
    ll->head = n;
    ll->tail = n;
  } else {
    n->prev = ll->tail;
    ll->tail = n;
    n->prev->next = n;
  }
}

node* getNodeAt(int i, llist* ll) {
  if (i<0 || ll == NULL || ll->head == NULL) return NULL;
  node* n = ll->head;
  int x = 0;
  while(x<i && n != NULL) {
    x++;
    n = n->next;
  }
  return n;
}

// Returns the index of the first matching string in the list.
// if there are no matches, returns -1
int find(char* s, llist* ll) {
  return 0; 
}

int deleteNode(int i, llist* ll) {
  node* n = getNodeAt(i, ll);
  if (n == NULL) return 0;
  if (n->next != NULL) n->next->prev = n->prev;
  if (n->prev != NULL) n->prev->next = n->next;
  if (ll->head == n) ll->head = n->next;
  if (ll->tail == n) ll->tail = n->prev;
  free(n);
  return 1;
}

void deleteAllNodes(llist* ll) {
  while(deleteNode(0, ll) == 1) { }
}

int main() {
  printf("got to the beginning\n");
  llist ll = {.head = NULL, .tail = NULL};
  append(&ll, "one");
  append(&ll, "two");
  append(&ll, "three");
  append(&ll, "four");
  append(&ll, "five");
  printll(&ll);

  deleteNode(2, &ll);

  printll(&ll);

  deleteNode(3, &ll);

  printll(&ll);

  deleteAllNodes(&ll);

  printll(&ll);

  append(&ll, "001");
  append(&ll, "002");
  append(&ll, "003");
  append(&ll, "004");
  append(&ll, "005");
  
  printill(&ll);

  deleteNode(-1, &ll);
  
  printill(&ll);

  deleteNode(5, &ll);
  
  printill(&ll);

  deleteNode(3, &ll);
  deleteNode(1, &ll);
  
  printill(&ll);
  
  printf("\nclean up\n");

  deleteAllNodes(&ll);

  printll(&ll);
  
  printf("\nDone.\n");
  
  return 0;
}
