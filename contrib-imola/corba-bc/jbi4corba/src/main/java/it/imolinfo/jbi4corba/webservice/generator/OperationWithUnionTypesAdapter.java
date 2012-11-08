 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;

import java.util.Map;


import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;


public class OperationWithUnionTypesAdapter extends ClassAdapter {

	private Map<String, UnionType> allUnionTypes;
	private String workingDirClasses;

	public OperationWithUnionTypesAdapter(ClassVisitor arg0,
			Map<String, UnionType> allUT, String workingDirCls) {
		super(arg0);
		allUnionTypes = allUT;
		workingDirClasses = workingDirCls;
	}

	@Override
	public MethodVisitor visitMethod(int access, String name, String desc,
			String signature, String[] exceptions) {

		try {
			desc = UnionTypeUtils.replaceUnionTypesInMethodSignature(desc, allUnionTypes, workingDirClasses);
			// contains holders
			if (signature != null)
			{
				signature = UnionTypeUtils.replaceUnionTypesInMethodSignature(signature, allUnionTypes, workingDirClasses);
			}
		} catch (ClassGenerationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return super.visitMethod(access, name, desc, signature, exceptions);
	}

	

}
