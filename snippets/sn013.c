#include <string.h>
#include <stdio.h>

typedef double (*adjustment_function) (double);

double my_double(double x) { return x + x; }
double my_square(double x) { return x * x; }
double my_same(double x)   { return x; }
double my_zero(double x)   { return 0; }

adjustment_function get_adjustment(const char * day) {
    if (strcmp(day, "Friday") == 0)
        return my_square; 
    else if (strcmp(day, "Monday") == 0)
        return my_zero; 
    else if (strcmp(day, "Thursday") == 0)
        return my_double; 
    else
        return my_same;
}

double adjust_salary(double salary, const char * day) {
    adjustment_function f = get_adjustment(day);
    return f(salary);
}

int main (void) {
    double salary = 15.0;
    printf("Old salary %f\n", salary);
    printf("New salary %f\n", adjust_salary(salary, "Thursday"));
    return 0;
}


