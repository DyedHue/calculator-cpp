#include <iostream>
#include <string>
#include <cmath>
using namespace std;

//-------------------
bool useRadian = true;
//-------------------
class number
{
public:
    double re = 0, im = 0;

    number() = default;
    number(double Re, double Im) : re(Re), im(Im) {}

    number operator+(const number& other) const
    {
        return {re + other.re, im + other.im};
    }
    number operator-(const number& other) const
    {
        return {re - other.re, im - other.im};
    }
    number operator*(const number& other) const
    {
        return {re * other.re - im * other.im, re * other.im + im * other.re};
    }
    number operator/(const number& other) const
    {
        double denom = other.re * other.re + other.im * other.im;
        double real = (re * other.re + im * other.im)/denom;
        double imag = (-re * other.im + im * other.re)/denom;
        return {real, imag};
    }
};
number complexPower(number base, number power)
{
    double r1 = sqrt(base.re*base.re + base.im * base.im);
    double r2 = sqrt(power.re*power.re + power.im * power.im);
    double theta1 = atan2(base.im, base.re);
    double theta2 = atan2(power.im, power.re);
    
    double newr = exp(r2*(log(r1) * cos(theta2) - theta1 * sin(theta2)));
    double newtheta = r2 * (log(r1) * sin(theta2) + theta1 * cos(theta2));

    return {newr*cos(newtheta), newr*sin(newtheta)};
}
double toradian(double n)
{
    if(useRadian) return n;
    return (n * 3.14159265358979323846)/180;
}
bool isInteger(double n)
{
    return abs(n - round(n)) <= 1e-9;
}
bool isEqual(double a, double b)
{
    return abs(a-b) <= 1e-9;
}
bool startsWith(const string& s, const string& pref)
{
    return s.compare(0, pref.size(), pref) == 0;
}
double strict_stod(const string& s)
{
    size_t endPos = 0;
    double val = stod(s, &endPos);

    if (endPos < s.size())
        throw invalid_argument("");
    return val;
}
long long factorial(int n)
{
    if(n == 0) return 1;
    long long ans = 1;
    for(int i = 1; i <= n; i++)
        ans*=i;
    return ans;
}
number evaluate(string s, double xValue = 0)
{
    number num;
    int plusminusPos = -1, multidivPos = -1, expPos = -1, comPermPos = -1;
    int parenthDepth = 0;
    bool insideAbs = 0;
    for(int i = s.size() - 1; i >= 0; i--)
    {
        if(s[i] == ')')
            parenthDepth++;
        else if(s[i] == '(')
            parenthDepth--;
        
        if(!parenthDepth)
        {
            if(!insideAbs)
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
            if(s[i] == '|')
                insideAbs = !insideAbs;
        }
    }
    if(plusminusPos != -1)
        if(s[plusminusPos] == '+') return evaluate(s.substr(0, plusminusPos), xValue) + evaluate(s.substr(plusminusPos + 1), xValue);
        else                       return evaluate(s.substr(0, plusminusPos), xValue) - evaluate(s.substr(plusminusPos + 1), xValue);
    if(multidivPos != -1)
        if(s[multidivPos] == '*') return evaluate(s.substr(0, multidivPos), xValue) * evaluate(s.substr(multidivPos + 1), xValue);
        else                      return evaluate(s.substr(0, multidivPos), xValue) / evaluate(s.substr(multidivPos + 1), xValue);
    if(expPos != -1)
    {
        number base = evaluate(s.substr(0, expPos), xValue);
        number power = evaluate(s.substr(expPos + 1), xValue);

        if(!isEqual(base.im,0) || !isEqual(power.im, 0) || (base.re < 0 && !isInteger(power.re)))
            return complexPower(base, power);
        
        return {pow(base.re, power.re), 0};
    }
        
    if(comPermPos != -1)
    {
        string operation = ((s[comPermPos] == 'c')? "combination" : "permutation");

        number nn = evaluate(s.substr(0, comPermPos), xValue);
        number nr = evaluate(s.substr(comPermPos + 1), xValue);
        if(!isEqual(nn.im, 0) || !isEqual(nr.im, 0))
        {
            throw runtime_error("Error: " + operation + " is not supported for complex numbers");
        }
        double n = nn.re;
        double r = nr.re;

        if(n < 0 || r < 0)
            throw runtime_error("Error: "+ operation +" for negative numbers is not supported");
        else if(!isInteger(n) || !isInteger(r))
            throw runtime_error("Error: "+ operation +" for fractions is not supported");
        else if(n < r)
            throw runtime_error("Error: n < r while calculating " + operation);
        else
        {
            if(s[comPermPos] == 'c') return {factorial(n)/(factorial(r) * factorial(n - r)), 0};
            else                     return {factorial(n)/factorial(n-r), 0};
        }
    }
    if(s[s.size() -1] == '!')
    {
        num = evaluate(s.substr(0, s.size() - 1), xValue);
        if(!isEqual(num.im, 0))
            throw runtime_error("Error: factorial is not supported for complex numbers");
        
        double rnum = num.re;
        if(rnum < 0)
            throw runtime_error("Error: Factorial for negative numbers is not supported");
        else if(!isInteger(rnum))
            throw runtime_error("Error: Factorial for fractions is not supported");
        else if(rnum > 20)
            throw runtime_error("Error: Factorial for numbers over 20 is too large");
        else return {factorial(rnum), 0};
    }
    if(s[s.size() -1] == '%')
        return evaluate(s.substr(0, s.size() - 1), xValue)/number(100.0, 0);
    if(s[0] == '|' && s[s.size()-1] == '|')
    {
        num = evaluate(s.substr(1, s.size() - 2), xValue);
        if(isEqual(num.im, 0))
            return {abs(num.re), 0};
        else
            return {sqrt(num.re*num.re + num.im*num.im), 0};
    }
    if(s[0] == '-') return number(-1, 0) * evaluate(s.substr(1), xValue);
    if(s[0] == '+') return evaluate(s.substr(1), xValue);

    
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
    if((pos == -1 && s[s.size()-1] == ')') || (pos != -1 && s[s.size()-1] != ')'))
        throw runtime_error("Error: Incomplete parentheses");
    if(pos != -1)
    {
        string funcName = s.substr(0, pos);
        if(funcName == "") return evaluate(s.substr(pos + 1, s.size() - (pos + 1) - 1), xValue);

        if(funcName == "int")
        {
            int comma1Pos = s.find(',', 4);
            int comma2Pos = s.find(',', comma1Pos+1);
            
            if(comma2Pos == string::npos)
                throw runtime_error("Syntax error! Use integration like this: int(lower_limit, upper_limit, function_of_x)");
            
            number nlow = evaluate(s.substr(4, comma1Pos - 4), xValue);
            number nhigh = evaluate(s.substr(comma1Pos + 1, comma2Pos - (comma1Pos + 1)), xValue);
            double low = nlow.re, high = nhigh.re;
            string xfunc = s.substr(comma2Pos + 1, s.size() -1 - (comma2Pos + 1));

            if(!isEqual(nlow.im, 0) || !isEqual(nhigh.im, 0)) throw runtime_error("Error: You can't use complex numbers for bounds in integration");
            int sliceNumber = 1e6;
            double stepAmount = (high - low)/sliceNumber;

            double ans = 0;
            double x = low;
            for(int i = 1; i < sliceNumber; i++)
            {
                ans += evaluate(xfunc, x).re;
                x += stepAmount;
            }
            ans*=stepAmount;
            return {ans, 0};
        }

        num = evaluate(s.substr(pos + 1, s.size() - (pos + 1) - 1), xValue);
        bool real = isEqual(num.im, 0);
        
        if(funcName == "sqrt")
        {
            if(real && num.re >= 0) return {sqrt(num.re), 0};
            else return complexPower(num, {0.5, 0});
        }
        if(funcName.size() >= 2 && startsWith(funcName, "rt_"))
        {
            number n = evaluate(funcName.substr(3), xValue);
            number power = number(1, 0)/n;

            if(!real || !isEqual(power.im, 0) || (num.re < 0 && !isInteger(power.re)))
                return complexPower(num, power);
            
            return {pow(num.re, 1/n.re), 0};
        }

        if(!real)
            throw runtime_error(funcName + "function does not support complex numbers");


        if(funcName == "log")
        {
            return {log10(num.re), 0};
        }
        if(funcName == "ln")
        {
            return {log(num.re), 0};
        }
        if(funcName.size() >= 4 && startsWith(funcName, "log_"))
        {
            double base = evaluate(funcName.substr(4), xValue).re;
            return {log(num.re)/log(base), 0};
        }
        if(funcName == "sin")
            return {sin(toradian(num.re)), 0};
        if(funcName == "cos")
            return {cos(toradian(num.re)), 0};
        if(funcName == "tan")
            return {tan(toradian(num.re)), 0};
        if(funcName == "asin")
            return {asin(toradian(num.re)), 0};
        if(funcName == "acos")
            return {acos(toradian(num.re)), 0};
        if(funcName == "atan")
            return {atan(toradian(num.re)), 0};

        throw runtime_error("Error: \"" + funcName + "\" is not an available function");
    }
    
    if(s[0] == 'x')
        return {xValue, 0};
    if(s[0] == 'i')
        return {0, 1};

    try
    {
        return {strict_stod(s), 0};
    }
    catch(invalid_argument& e)
    {
        throw runtime_error("Syntax error!");
    }
}
int main()
{
    cout << "Calculator started! Enter 'm' for manual.\n\n";
    string ss, s;
    while(getline(cin, ss))
    {
        s = "";
        for(int i = 0; i < ss.size(); i++)
            if(ss[i] != ' ')
                s+=tolower(static_cast<unsigned char>(ss[i]));
        if(s == "m")
        {
            cout <<"Write an expression like (3.2 - 5^2 + 45.34)/(8*6) and press Enter to get the result (spaces are ignored)\n\nSupported operations:\n    Addition       +\n    Subtraction    -\n    Multiplication *\n    Division       /\n    Exponent       ^\n    Factorial      !\n    Percentage     %\n    Permutation    p\n    Combination    c\n\nSupported functions:\n    sin(x)\n    cos(x)\n    tan(x)\n    asin(x)   arcsin\n    acos(x)   arccros\n    atan(x)   arctan\n    ln(x)\n    log(x)    10 based log\n    log_b(x)  b based log of x\n    sqrt(x)   square root\n    rt_n(x)   nth root of x\n    |x|       absolute value\n\nIntegration:\n    int(l, h, f)   integrate w.r.t x\n\n    Here l is the lower bound, h is the upper bound and f is where you put your function of x like this: 3*x + 2.4*x^2. (Don't add \"dx\" at the end)\n\nComplex numbers are supported for +,  -,  *,  /,  ^, ||, sqrt() and rt_n() currently. You can also write 'i' in the expression for sqrt(-1)\nYou can also write in scientific notation (e.g., 4e6 will be considered 4000000)\nCurrently, you need to write multiplication explicitly for everything such as 3*i, (4+5)*(6-9) or 5*x\nRoots don't guarantee the principal root for imaginary numbers currently\nThere is currently no overflow checks\n\n";
            continue;
        }
        if(s == "q") break;
        try
        {
            number ans = evaluate(s);
            if(isEqual(ans.re, 0)) ans.re = 0;
            if(isEqual(ans.im, 0)) ans.im = 0;
            
            cout << "= ";
            if(ans.re!=0)
                cout << ans.re;
            if(ans.im!=0)
            {
                if(ans.re!=0)
                {
                    cout << " ";
                    if(ans.im >= 0)
                        cout << "+";
                }
                if(isEqual(abs(ans.im), 1)) cout << (ans.im > 0? "i":"-i");
                else cout << ans.im << "i";
            }
            if(ans.re == 0 && ans.im == 0)
                cout << "0";

            cout << "\n\n";
        }
        catch(const exception& e)
        {
            cerr << e.what() << "\n\n";
        }
    }
}