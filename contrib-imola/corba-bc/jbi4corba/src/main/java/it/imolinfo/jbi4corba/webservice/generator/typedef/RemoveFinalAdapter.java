package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Removes the <code>final</code> class modifier to extend the class.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */

public class RemoveFinalAdapter extends ClassAdapter {
	
	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(RemoveFinalAdapter.class);
	
	public RemoveFinalAdapter(ClassVisitor cv) {
		super(cv);
	}
	
	@Override
	public void visit(int version, int access, String name, String signature,
			String superName, String[] interfaces) {
	    // Remove the "final". Probably the analog operation in the IdlToWsdl adapter is the useless.
	    if ((Opcodes.ACC_FINAL & access) == Opcodes.ACC_FINAL) {
	        access = access & (~Opcodes.ACC_FINAL);
	        LOG.debug("Removed final from class");
	    }
		super.visit(version, access, name, signature, superName, interfaces);

	}	

}
