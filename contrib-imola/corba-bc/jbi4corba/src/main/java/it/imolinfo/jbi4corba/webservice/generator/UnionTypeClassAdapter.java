 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.util.Set;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;


/**
 * This ASM Adapter class changes the 'private' to 'public' modifier 
 * for all verifyXXX methods of union types for each field
 */
public class UnionTypeClassAdapter extends ClassAdapter {

	private Set<String> listOfFields = null;
	
	@Override
	public MethodVisitor visitMethod(int access, String name, String desc,
		    String signature, String [] exceptions) {
	
		
		if (name.startsWith("verify") && access == Opcodes.ACC_PRIVATE && name.length() > 6)
		{
			String field = name.substring(6);
			if (listOfFields.contains(field))
				access = Opcodes.ACC_PUBLIC;
		}
		return super.visitMethod(access, name, desc, signature, exceptions);
	}

	public UnionTypeClassAdapter(ClassVisitor arg0, Set<String> typeFieldNameList) {
		super(arg0);
		listOfFields = typeFieldNameList;
	}

}
