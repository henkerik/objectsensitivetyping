/*
 * phc -- the open source PHP compiler
 * See doc/license/README.license for licensing information
 *
 * Remove all the phc.unparser attributes from the IR
 */

#include <AST.h>
#include <AST_transform.h>
#include <AST_visitor.h>
#include <pass_manager/Plugin_pass.h>
#include <process_ir/General.h>
#include <process_ir/fresh.h>

using namespace AST;

namespace AST
{
    class Lower_expr : public Transform, virtual public GC_obj
    {
    public:
    	void children_php_script(PHP_script* in)
    	{
    	    pieces = new Statement_list;
        	Transform::children_php_script(in);
    	}

    public:
    	void children_if (If* in)
    	{
    	    in->expr = transform_expr(in->expr);

        	backup_pieces();
        	in->iftrue = transform_statement_list(in->iftrue);
        	in->iffalse = transform_statement_list(in->iffalse);
        	restore_pieces();
    	}
    	void post_if (If* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}

    	void children_while (While* in)
    	{
    	    in->expr = transform_expr(in->expr);

        	backup_pieces();
        	in->statements = transform_statement_list(in->statements);
        	restore_pieces();
    	}
    	void post_while (While* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}

    	void children_foreach (Foreach* in)
    	{
    	    in->expr = transform_expr(in->expr);

        	backup_pieces();
        	in->statements = transform_statement_list(in->statements);
        	restore_pieces();
    	}
    	void post_foreach (Foreach* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}

    	// Switch is already lowered
    	// Do is already lowered
    	// For is already lowered
    	// Try blocks don't have expressions so don't need special treatment
    	// Static declarations cannot be lowered

    	void post_eval_expr (Eval_expr* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}
    	void post_return (Return* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}
    	void post_global (Global* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}
    	void post_continue (Continue* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}
    	void post_break (Break* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}
    	void post_throw (Throw* in, Statement_list* out)
    	{
    	    push_back_pieces(in, out);
    	}

    // Common code generation patterns
    protected:
    	Expr* eval(Expr* in)
    	{
    	    if(in->attrs->is_true("phc.ast_lower_expr.no_temp"))
        	{
        		return in;
        	}
        	else
        	{
        		Variable* temp = fresh_var("TLE"); 
        		eval(in, temp);
        		return temp;
        	}
    	}
    	void eval(Expr* in, Variable* temp)
    	{
    	    pieces->push_back(new Eval_expr(new Assignment(temp->clone (), false, in->clone ())));
    	}

    // Define a number of hooks that can be redefined by inheriting classes
    // to tweak the behaviour of the transformation
    protected:
    	virtual void push_back_pieces(Statement* in, Statement_list* out)
    	{
    	    out->push_back_all(pieces);
        	out->push_back(in);

        	// Move comment to the first piece (if any)
        	if(!pieces->empty())
        	{
        		pieces->front()->attrs->set("phc.comments", in->get_comments());
        		in->attrs->set("phc.comments", new String_list);
        	}

        	pieces->clear();
    	}
    	virtual void backup_pieces()
    	{
    	    pieces_backup.push(pieces);
        	pieces = new Statement_list;
    	}
    	virtual void restore_pieces()
    	{
    	    pieces = pieces_backup.top();
        	pieces_backup.pop();
    	}

    protected:
    	Statement_list* pieces;
    	Stack<Statement_list*> pieces_backup;
    };
}

class Annotate : public virtual AST::Visitor, virtual public GC_obj
{
    protected:
        bool generate_array_temps;
    
    public:
        Annotate() : generate_array_temps(true) 
        {   

        }
        /*
        void pre_name_with_default (Name_with_default* in)
        {
        	// Never generate a temp for a default value
        //	if(in->expr)
        //		in->expr->attrs->set_true("phc.ast_lower_expr.no_temp");
        }
        
        void pre_attribute(Attribute* in)
        {
        	generate_array_temps = false;
        }   

        void post_attribute(Attribute* in)
        {
        	generate_array_temps = true;
        }
        /*
        
        void pre_array_elem (Array_elem* in)
        {
        	if (generate_array_temps == false)
        	{
        		if(in->key)
        			in->key->attrs->set_true("phc.ast_lower_expr.no_temp");
        		if(in->val)
        			in->val->attrs->set_true("phc.ast_lower_expr.no_temp");
        	}
        }*/
};

class LowerArray : public virtual AST::Lower_expr, virtual public GC_obj
{	
protected:
    bool isEnabled;
    
public:
    void children_php_script(PHP_script* in)
    {    
    	//Annotate ann;
    	//in->visit(&ann);
    	
        isEnabled = false;

    	Lower_expr::children_php_script(in);
    }
    
    void children_assignment(Assignment* in)
    {
        isEnabled = true;
        in->variable = transform_variable(in->variable);
        isEnabled = false;
        in->expr = transform_expr(in->expr);
    }
    
    Expr* post_int(INT* in)
    {
        if (isEnabled) {   
    	    return eval(in);
    	} else {
            return in;
    	}
    }
};

/**
 * The Shredder::post_array method transforms an array declaration:
 *
 * $a = array ('one', 'two', 'three');
 *
 * To:
 *
 * unset ($TSa1);
 * $TSa[0] = 'one';
 * $TSa[1] = 'two';
 * $TSa[2] = 'three';
 *
 * However, a scalar as an index (e.g the 0,1 and 2) are not allowed in our IR. 
 * This filter lowers the result of Shredder::post_array even further to:
 * 
 * unset ($TSa1);
 * $TLE1 = 0;
 * $TSa[$TLE1] = 'one';
 * $TLE2 = 1;
 * $TSa[$TLE2] = 'two';
 * $TLE3 = 2;
 * $TSa[$TLE3] = 'three';
 */

extern "C" void load (Pass_manager* pm, Plugin_pass* pass)
{
        pm->add_after_named_pass (pass, new String ("ashred"));
}

extern "C" void run_ast (AST::PHP_script* in, Pass_manager* pm, String* option)
{
        LowerArray transform;
        in->transform_children(&transform);
}