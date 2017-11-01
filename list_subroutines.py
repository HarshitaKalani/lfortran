from liblfort.fortranParser import fortranParser
from liblfort.fortranVisitor import fortranVisitor
from liblfort.fortran_parser import get_parser

class SubroutinesVisitor(fortranVisitor):

    # Visit a parse tree produced by fortranParser#subroutine.
    def visitSubroutine(self, ctx:fortranParser.SubroutineContext):
        print("subroutine %s(%s)" % (ctx.ID()[0].getText(),
            ctx.id_list().getText()))
        return self.visitChildren(ctx)

def main():
    filename = "examples/random.f90"
    source = open(filename).read()
    parser = get_parser(source)
    tree = parser.root()
    v = SubroutinesVisitor()
    v.visit(tree)

if __name__ == "__main__":
    main()
