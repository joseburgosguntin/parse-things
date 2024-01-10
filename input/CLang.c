#include <stdio.h>

struct Person {
  char *name;
  int age, height;
};

void print_person(struct Person person) {
  printf("Name: %s\n", person.name);
  printf("Age: %d\n", person.age);
  printf("Height: %d\n", person.height);
}

int main(int argc, char *argv[]) {
  struct Person person;
  printf("What's your name?\n");
  scanf("%s", person.name);

  printf("What's your age?\n");
  scanf("%d", &person.age);

  printf("What's your height?\n");
  scanf("%d", &person.height);

  print_person(person);

  return 0;
}
