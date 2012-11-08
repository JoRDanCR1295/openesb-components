 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;


import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;

/**
 * This class is to modify the all operations that contain any as parameter/return type
 * Replace: Any with type Object the code to initialize some fields.
 * 
 * @author laurLG
 */
public class OperationWithAnyTypesAdapter extends ClassAdapter {


	public OperationWithAnyTypesAdapter(ClassVisitor arg0) {
		super(arg0);
	}

	@Override
	public MethodVisitor visitMethod(int access, String name, String desc,
			String signature, String[] exceptions) {

		try {
			desc = AnyTypeUtils.replaceAnyTypesInMethodSignature(desc);
			// contains holders
			if (signature != null)
			{
				signature = AnyTypeUtils.replaceAnyTypesInMethodSignature(signature);
			}
		} catch (ClassGenerationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return super.visitMethod(access, name, desc, signature, exceptions);
	}

	

}
