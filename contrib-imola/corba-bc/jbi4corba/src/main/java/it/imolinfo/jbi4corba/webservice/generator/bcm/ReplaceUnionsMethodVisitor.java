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
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.UnionTypeUtils;

import java.util.Map;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.TraceMethodVisitor;

/**
 * This class is to modify the all methods that contain uniontype attribution
 * Replace: Uniontype with type Object the code to initialize some fields.
 */
public class ReplaceUnionsMethodVisitor extends TraceMethodVisitor {

	

	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ConstructorModMethodVisitor.class);

	private Map<String, UnionType> allUnionTypes;

	/**
	 * Constructor.
	 * 
	 * @param mv
	 *            The method visitor.
	 * @param mapOfFields
	 *            The fields to initialize.
	 * @param className
	 *            The name of the class.
	 * @param allInterfaceTypes
	 */
	public ReplaceUnionsMethodVisitor(MethodVisitor mv,
			Map<String, UnionType> allUnionTypes) {
		super(mv);

		this.allUnionTypes = allUnionTypes;
	}

	/**
	 * This method visit the code of the methods and change type of union with wrappers
	 * 
	 */

	public void visitFieldInsn(int opcode, String className, String fieldName,
			String fieldType) {

		LOG.debug("<<<<< AppenderMethodVisitor.visitFieldInsn - end");
		if (opcode == Opcodes.PUTFIELD || opcode == Opcodes.GETFIELD) {
			boolean isArray = TypeUtils.isArray(fieldType);
			UnionType unionField = UnionTypeUtils.isUnionType(fieldType, isArray, allUnionTypes);
			if ( unionField != null) {
				String arrayStr = TypeUtils.getArrayDimmentionAsPrefix(fieldType);
				
				fieldType = arrayStr + "L" + unionField.getTypeName().replace('.', '/') + "Wrapper;";

				
			}
		}
		super.visitFieldInsn(opcode, className, fieldName, fieldType);

	}
	
	

}
