/*
 * phc -- the open source PHP compiler
 * See doc/license/README.license for licensing information
 *
 * Remove all the phc.unparser attributes from the IR
 */

#include <AST.h>
#include <AST_visitor.h>
#include <pass_manager/Plugin_pass.h>

using namespace AST;

class PostAshred : public virtual AST::Visitor, virtual public GC_obj
{
	void post_node (AST::Node* in)
	{
        in->attrs->erase ("phc.unparser.needs_user_brackets");
	}
};

extern "C" void load (Pass_manager* pm, Plugin_pass* pass)
{
        pm->add_after_named_pass (pass, new String ("ashred"));
}

extern "C" void run_ast (AST::PHP_script* in, Pass_manager* pm, String* option)
{
        PostAshred v;
        in->visit(&v);
}