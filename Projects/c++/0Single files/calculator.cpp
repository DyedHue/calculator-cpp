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
double strict_stod(const string& s)
{
    size_t endPos = 0;
    double val = stod(s, &endPos);

    if (endPos < s.size()) {
        throw invalid_argument("");
    }
    return val;
}

string plug_value(string func, double X)
{
    string x = to_string(X);
    int xpos;
    while((xpos = func.find('x', xpos)) != string::npos)
    {
        func.replace(xpos, 1, x);
        xpos += x.size();
    }
    return func;
}
long long factorial(int n)
{
    if(n == 0) return 1;
    long long ans = 1;
    for(int i = 1; i <= n; i++)
        ans*=i;
    return ans;
}
double evaluate(string s, double xValue = 0)
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
            if((s[i] == '+' || s[i] == '-') && i != 0 && s[i-1] != '+' && s[i-1] != '-' && s[i-1] != '*' && s[i-1] != '/' && s[i-1] != '^' && s[i-1] != 'e' && plusminusPos == -1)
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
        if(s[plusminusPos] == '+') return evaluate(s.substr(0, plusminusPos), xValue) + evaluate(s.substr(plusminusPos + 1), xValue);
        else                       return evaluate(s.substr(0, plusminusPos), xValue) - evaluate(s.substr(plusminusPos + 1), xValue);
    else if(multidivPos != -1)
        if(s[multidivPos] == '*') return evaluate(s.substr(0, multidivPos), xValue) * evaluate(s.substr(multidivPos + 1), xValue);
        else                      return evaluate(s.substr(0, multidivPos), xValue) / evaluate(s.substr(multidivPos + 1), xValue);
    else if(expPos != -1)
        return pow(evaluate(s.substr(0, expPos), xValue), evaluate(s.substr(expPos + 1), xValue));
    else if(comPermPos != -1)
    {
        string operation = ((s[comPermPos] == 'c')? "combination" : "permutation");

        double n = evaluate(s.substr(0, comPermPos), xValue);
        double r = evaluate(s.substr(comPermPos + 1), xValue);
        if(n < 0 || r < 0)
            throw runtime_error("Error: "+ operation +" for negative numbers is not supported");
        else if(!isInteger(n) || !isInteger(r))
            throw runtime_error("Error: "+ operation +" for fractions is not supported");
        else if(n < r)
            throw runtime_error("Error: n < r while calculating " + operation);
        else
        {
            if(s[comPermPos] == 'c') return factorial(n)/(factorial(r) * factorial(n - r));
            else                     return factorial(n)/factorial(n-r);
        }
    }

    else if(s[s.size() -1] == '!')
    {
        num = evaluate(s.substr(0, s.size() - 1), xValue);
        if(num < 0)
            throw runtime_error("Error: Factorial for negative numbers is not supported");
        else if(!isInteger(num))
            throw runtime_error("Error: Factorial for fractions is not supported");
        else if(num > 20)
            throw runtime_error("Error: Factorial for numbers over 20 is too large");
        else return factorial(num);
    }
    else if(s[s.size() -1] == '%')
        return evaluate(s.substr(0, s.size() - 1), xValue)/100.0;
    else
    {
        if(s[0] == '-') return -evaluate(s.substr(1), xValue);
        
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
            string funcName = s.substr(0, pos);

            if(funcName == "int")
            {
                int comma1Pos = s.find(',', 4);
                int comma2Pos = s.find(',', comma1Pos+1);
              
                if(comma2Pos == string::npos)
                    throw runtime_error("Syntax error! Use integration like this: int(lower_limit, upper_limit, function_of_x)");
                
                double low = evaluate(s.substr(4, comma1Pos - 4), xValue);
                double high = evaluate(s.substr(comma1Pos + 1, comma2Pos - (comma1Pos + 1)), xValue);
                string xfunc = s.substr(comma2Pos + 1, s.size() -1 - (comma2Pos + 1));
                int sliceNumber = 1e6;

                double stepAmount = (high - low)/sliceNumber;

                double ans = 0;
                double x = low;
                for(int i = 1; i < sliceNumber; i++)
                {
                    // ans += evaluate(plug_value(xfunc, x));
                    ans += evaluate(xfunc, x);
                    x += stepAmount;
                }
                ans*=stepAmount;
                return ans;
            }

            num = evaluate(s.substr(pos + 1, s.size() - (pos + 1) - 1), xValue);
    
            if(funcName == "sin")
                return sin(toradian(num));
            if(funcName == "cos")
                return cos(toradian(num));
            if(funcName == "tan")
                return tan(toradian(num));
            if(funcName == "asin")
                return asin(toradian(num));
            if(funcName == "acos")
                return acos(toradian(num));
            if(funcName == "atan")
                return atan(toradian(num));
            if(funcName == "log")
                return log10(num);
            if(funcName == "ln")
                return log(num);
            if(funcName == "sqrt")
                return sqrt(num);
            if(funcName.size() >= 4 && startsWith(funcName, "log_"))
            {
                double base = evaluate(funcName.substr(4), xValue);
                return log(num)/log(base);
            }
            if(funcName.size() >= 2 && startsWith(funcName, "rt_"))
            {
                double n = evaluate(funcName.substr(3), xValue);
                return pow(num, 1/n);
            }
            if(funcName != "")
                throw runtime_error("Error: \"" + funcName + "\" is not an available function");
            
            return num;
        }
        
        if(s[0] != 'x')
        {
            try
            {
                return strict_stod(s);
            }
            catch(invalid_argument& e)
            {
                throw runtime_error("Syntax error!");
            }
        }
        else
        {
            return xValue;
        }
    }
}
int main()
{
    // cout << "Calculator started! Enter 'm' for manual.\n\n";
    // while(1)
    {
        string ss, s ="";
        getline(cin, ss);
        for(int i = 0; i < ss.size(); i++)
            if(ss[i] != ' ')
                s+=ss[i];
        if(s == "m")
        {
            cout <<"Write an expression like (3.2 - 5^2 + 45.34)/(8*6) and press Enter to get the result (spaces are ignored)\n\nSupported operations:\n    Addition       +\n    Subtraction    -\n    Multiplication *\n    Division       /\n    Exponent       ^\n    Factorial      !\n    Percentage     %\n    Permutation    p\n    Combination    c\n\nSupported functions: (Always use parentheses around the value)\n    sin(x)\n    cos(x)\n    tan(x)\n    ln(x)\n    log(x)    10 based log\n    log_b(x)  b based log of x\n    sqrt(x)   square root\n    rt_n(x)   nth root of x\n\nYou can also write in scientific notation (e.g., 4e6 will be considered 4000000)\n\n";
            // continue;
        }
        try
        {
            cout << "= " << evaluate(s) << "\n\n";
        }
        catch(const std::exception& e)
        {
            cerr << e.what() << "\n\n";
        }
    }
}