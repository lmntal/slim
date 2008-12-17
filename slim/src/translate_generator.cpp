#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include <vector>

using namespace std;

void replace_string(string& s, const string& from, const string& to)
{
    string::size_type p = 0;

    while((p=s.find(from, p)) != string::npos) {
        s.replace(p, from.size(), to);
        p += to.size();
    }
}

int main() {
    string s;
    /*    
    while(getline(cin, s)){
        if(s[s.size()-1] == '\r'){
            s = s.substr(0, s.size()-1);
        }

        if(s.size() == 0){
            cout << "\tbreak;" << endl << "}" << endl;
        }else if(s[0] == '#'){
            string::size_type opend = s.find(" ", 1);
            if(opend == string::npos) opend = s.size();
            string op = s.substr(1, opend-1);
            transform(op.begin(), op.end(), op.begin(), (int (*)(int))toupper);
            cout << "case INSTR_" << op << ":" << endl << "{" << endl;
            
            int arg_number = 0;
            for(string::size_type p=opend+1; p<s.size(); p+=2,++arg_number){
                switch(s[p]){
                  case 'v':
                  case 'i':
                    {
                        cout << "\tint arg"<<arg_number<<" = *((int*)instr.args["<<arg_number<<"].v);" << endl;
                        break;
                    }
                  case 'f':
                    {
                        cout << "\tstruct Functor arg"<<arg_number<<" = *(struct Functor*)instr.args["<<arg_number<<"].v;" << endl;
                        break;
                    }
                  default:
                    {
                        cerr << "unknown arg type '"<<s[p]<<"'" << endl;
                        return 1;
                        break;
                    }
                }
            }
            
            cout << endl;
        }else if(s[0] == '%'){
            s = s.substr(1);
            cout << s << endl;
        }else{
            replace_string(s, "\\", "\\\\");
            replace_string(s, "\"", "\\\"");
            replace_string(s, "'", "\\'");
            replace_string(s, "%", "%%");
            
            vector<string> inline_arg;
            string::size_type p = 0;
            while((p=s.find("$$", p)) != string::npos){
                if(s[p+2] == 'f'){
                    inline_arg.push_back("fail_code");
                    s.replace(p, 3, "%s");
                }else if(s[p+2] == 'p'){
                    inline_arg.push_back("finish_code");
                    s.replace(p, 3, "%s");
                }else if(s[p+2] == '('){
                    string::size_type arg_begin = p+3;
                    string::size_type arg_end = s.find(")", arg_begin);
                    string::size_type arg_length = arg_end - arg_begin;
                    inline_arg.push_back(s.substr(arg_begin, arg_length));
                    s.replace(p, arg_length+4, "%ld");
                }else{
                    inline_arg.push_back(string("arg") + s[p+2]);
                    s.replace(p, 3, "%ld");
                }
            }
            
            cout << "\toutput_c_indent(indent);" << endl;
            cout << "\tprintf(\"" << s << "\\n\"";
            for(int i=0; i<inline_arg.size(); ++i){
                cout << ", " << inline_arg[i];
            }
            cout << ");" << endl;
        }
    }
    
    return 0;
    */
}
