 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.generator.AnyType;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.util.TraceMethodVisitor;

/**
 * This class is to modify the all methods that contain any attribution
 * Replace: Any with type Object the code to initialize some fields.
 * 
 * @author laurLG
 */
public class ReplaceAnyMethodVisitor extends TraceMethodVisitor {

	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ConstructorModMethodVisitor.class);

	/**
	 * Constructor.
	 * 
	 * @param mv
	 *            The method visitor.
	 */
	public ReplaceAnyMethodVisitor(MethodVisitor mv) {
		super(mv);
	}

	/**
	 * This method visit the code of the constructor and change Any
	 * with Object
	 * 
	 */

	public void visitFieldInsn(int opcode, String className, String fieldName,
			String fieldType) {

		LOG.debug("<<<<< AppenderMethodVisitor.visitInsn - end");
		if (opcode == Opcodes.PUTFIELD) {
			
			String arrayStr = TypeUtils.getArrayDimmentionAsPrefix(fieldType);
			boolean isAny = TypeUtils.getTypeFromTypeDescription(fieldType).equals(AnyType.CORBA_ANY_TYPE);
			if (isAny) {
				
					fieldType = arrayStr + "Ljava/lang/Object;";
			}
		}
		super.visitFieldInsn(opcode, className, fieldName, fieldType);

	}

}
