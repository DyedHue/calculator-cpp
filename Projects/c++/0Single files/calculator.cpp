#include <iostream>
#include <string>
#include <cmath>
using namespace std;

//-------------------
bool useRadian = true;
//-------------------
double toradian(double n)
{
    if(useRadian) return n;
    return (n * 3.14159265358979323846)/180;
}
bool isInteger(double n)
{
    return abs(n - round(n)) <= 1e-9;
}
bool startsWith(string s, string pref)
{
    return s.compare(0, pref.size(), pref) == 0;
}
long long factorial(int n)
{
    if(n == 0) return 1;
    long long ans = 1;
    for(int i = 1; i <= n; i++)
        ans*=i;
    return ans;
}
double evaluate(string s)
{
    double num;
    int plusminusPos = -1, multidivPos = -1, expPos = -1, comPermPos = -1;
    int parenthDepth = 0;
    for(int i = s.size() - 1; i >= 0; i--)
    {
        if(s[i] == ')')
            parenthDepth++;
        else if(s[i] == '(')
            parenthDepth--;
        
        if(!parenthDepth)
        {
            if((s[i] == '+' || s[i] == '-') && i != 0 && s[i-1] != '+' && s[i-1] != '-' && s[i-1] != '*' && s[i-1] != '/' && s[i-1] != '^' && plusminusPos == -1)
                plusminusPos = i;
            else if((s[i] == '*' || s[i] == '/') && multidivPos == -1)
                multidivPos = i;
            else if(s[i] == '^')
                expPos = i;
            else if((s[i] == 'c' || s[i] == 'p') && comPermPos == -1)
                comPermPos = i;
        }
    }
    if(plusminusPos != -1)
        if(s[plusminusPos] == '+') num = evaluate(s.substr(0, plusminusPos)) + evaluate(s.substr(plusminusPos + 1));
        else                       num = evaluate(s.substr(0, plusminusPos)) - evaluate(s.substr(plusminusPos + 1));
    else if(multidivPos != -1)
        if(s[multidivPos] == '*') num = evaluate(s.substr(0, multidivPos)) * evaluate(s.substr(multidivPos + 1));
        else                      num = evaluate(s.substr(0, multidivPos)) / evaluate(s.substr(multidivPos + 1));
    else if(expPos != -1)
        num = pow(evaluate(s.substr(0, expPos)), evaluate(s.substr(expPos + 1)));
    else if(comPermPos != -1)
    {
        string operation = ((s[comPermPos] == 'c')? "combination" : "permutation");

        double n = evaluate(s.substr(0, comPermPos));
        double r = evaluate(s.substr(comPermPos + 1));
        if(n < 0 || r < 0)
            throw runtime_error("Error: "+ operation +" for negative numbers is not supported");
        else if(!isInteger(n) || !isInteger(r))
            throw runtime_error("Error: "+ operation +" for fractions is not supported");
        else if(n < r)
            throw runtime_error("Error: n < r while calculating " + operation);
        else
        {
            if(s[comPermPos] == 'c') num = factorial(n)/(factorial(r) * factorial(n - r));
            else                     num = factorial(n)/factorial(n-r);
        }
    }

    else if(s[s.size() -1] == '!')
    {
        num = evaluate(s.substr(0, s.size() - 1));
        if(num < 0)
            throw runtime_error("Error: Factorial for negative numbers is not supported");
        else if(!isInteger(num))
            throw runtime_error("Error: Factorial for fractions is not supported");
        else if(num > 20)
            throw runtime_error("Error: Factorial for numbers over 20 is too large");
        else num = factorial(num);
    }
    else if(s[s.size() -1] == '%')
        num = evaluate(s.substr(0, s.size() - 1))/100.0;
    else
    {
        bool negative = false;
        if(s[0] == '-') s = s.substr(1), negative = true;
        int pos = -1;

        if(s.size() > 1)
        {
            int parenthDepth = 1;
            for(int i = s.size()-2; i >=0; i--)
            {
                if(s[i] == ')')
                    parenthDepth++;
                else if(s[i] == '(')
                {
                    parenthDepth--;
                    if(parenthDepth == 0)
                    {
                        pos = i;
                        break;
                    }
                }
            }
        }
        
        if(pos != -1)
        {
            num = evaluate(s.substr(pos + 1, s.size() - (pos + 1) - 1));
            string funcName = s.substr(0, pos);
    
            if(funcName == "sin")
                num = sin(toradian(num));
            else if(funcName == "cos")
                num = cos(toradian(num));
            else if(funcName == "tan")
                num = tan(toradian(num));
            else if(funcName == "log")
                num = log10(num);
            else if(funcName == "ln")
                num = log(num);
            else if(funcName == "sqrt")
                num = sqrt(num);
            else if(funcName.size() >= 4 && startsWith(funcName, "log_"))
            {
                double base = evaluate(funcName.substr(4));
                num = log(num)/log(base);
            }
            else if(funcName.size() >= 2 && startsWith(funcName, "rt_"))
            {
                double n = evaluate(funcName.substr(3));
                num = pow(num, 1/n);
            }
            else if(funcName != "")
                throw runtime_error("\"" + funcName + "\" is not an available function");
        }
        else
        {
            try
            {
                num = stod(s);
            }
            catch(invalid_argument& e)
            {
                throw runtime_error("Syntax error!");
            }
        }
        if(negative) num = -num;
    }
    return num;
}
int main()
{
    cout << "Calculator started! Enter 'm' for manual.\n\n";
    while(1)
    {
        string ss, s ="";
        getline(cin, ss);
        for(int i = 0; i < ss.size(); i++)
            if(ss[i] != ' ')
                s+=ss[i];
        if(s == "m")
        {
            cout <<"Write an expression like (3.2 - 5^2 + 45.34)/(8*6) and press Enter to get the result (spaces are ignored)\n\nSupported operations:\n    Addition       +\n    Subtraction    -\n    Multiplication *\n    Division       /\n    Exponent       ^\n    Factorial      !\n    Percentage     %\n    Permutation    p\n    Combination    c\n\nSupported functions: (Always use parentheses around the value)\n    sin(x)\n    cos(x)\n    tan(x)\n    ln(x)\n    log(x)    10 based log\n    log_b(x)  b based log of x\n    sqrt(x)   square root\n    rt_n(x)   nth root of x\n\n";
            continue;
        }
        try
        {
            cout << "= " << evaluate(s) << "\n\n";
        }
        catch(const std::exception& e)
        {
            std::cerr << e.what() << "\n\n";
        }
    }
}