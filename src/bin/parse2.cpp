#include <iostream>
#include <fstream>
#include <chrono>
#include <lfortran/parser/parser.h>
#include <lfortran/pickle.h>

int main(int argc, char *argv[])
{
    int N;
    N = 10000;
    std::string text;
    std::string st1 = "subroutine g";
    std::string st2 = R"(
    integer :: x, i
    x = 1
    do i = 1, 10
        x = x*i
    end do
end subroutine)";
    text.reserve(2250042);
    text = st1 + std::to_string(0) + st2;
    std::cout << "Construct" << std::endl;
    for (int i = 0; i < N-1; i++) {
        text.append("\n\n" + st1 + std::to_string(i+1) + st2);
    }
    {
        std::ofstream file;
        file.open("bench.f90");
        file << text;
    }

    Allocator al(64*1024*1024); // The actual size is 31,600,600
    std::cout << "Parse" << std::endl;
    auto t1 = std::chrono::high_resolution_clock::now();
    auto result = LFortran::parsen(al, text);
    auto t2 = std::chrono::high_resolution_clock::now();

    std::string p = LFortran::pickle(*result[0]);
    std::cout << "Number of units: " << result.size() << std::endl;

    std::cout << "Parsing: " <<
        std::chrono::duration_cast<std::chrono::milliseconds>(t2 - t1).count()
        << "ms" << std::endl;
    std::cout << "String size (bytes):      " << text.size() << std::endl;
    std::cout << "Allocator usage (bytes): " << al.size_current() << std::endl;


    return 0;
}
