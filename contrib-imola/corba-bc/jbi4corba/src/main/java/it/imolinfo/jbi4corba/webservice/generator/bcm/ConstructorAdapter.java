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
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;

import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Type;

/**
 * For each constructor replace all parameter interface type with W3CEndpointReference.
 */
public class ConstructorAdapter extends ClassAdapter {

	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ConstructorAdapter.class);

	protected String className = null;

	protected ClassWriter classWriter = null;

	protected Map<String, String> mapOfFields = new HashMap<String, String>();

	// True if the class is an Exception
	private boolean isException = false;

	private Map<String, InterfaceType> allInterfaceTypes;

	/**
	 * The adapter used to manipulate the code.
	 * 
	 * @param cv
	 *            The ClassVisitor used in this object.
	 * @param cw
	 *            The ClassWriter used in this object.
	 * @param cn
	 *            The class name to manipulate.
	 */
	public ConstructorAdapter(ClassVisitor cv, ClassWriter cw, String cn,
			Map<String, InterfaceType> allInTypes, boolean isEx) {

		super(cv);

		classWriter = cw;
		className = cn;
		isException = isEx;
		allInterfaceTypes = allInTypes;

		LOG.debug("CRB000604_new_GetterSetterAdapter", new Object[] { cv, cw,
				cn, isEx });
	}

	/**
	 * Override.
	 * 
	 * @param version
	 *            The method visitor
	 * @param access
	 *            The access
	 * @param name
	 *            The name
	 * @param signature
	 *            The signature
	 * @param superName
	 *            The super name
	 * @param interfaces
	 *            The interfaces
	 */

	@Override
	public void visit(int version, int access, String name, String signature,
			String superName, String[] interfaces) {
		LOG.debug(">>>>> visit - begin");

		LOG.debug("CRB000603_VISIT", new Object[] { version, access, name,
				signature, superName, interfaces });

		super.visit(version, access, name, signature, superName, interfaces);

	}

	/**
	 * Override.
	 * 
	 * @param access
	 *            The access
	 * @param name
	 *            The name
	 * @param desc
	 *            The description
	 * @param signature
	 *            The signature
	 * @param exceptions
	 *            The exceptions
	 * @return The return
	 */
	@Override
	public MethodVisitor visitMethod(int access, String name, String desc,
			String signature, String[] exceptions) {

		LOG.debug(">>>>> visitMethod - begin");

		LOG.debug("visitMethod. access=" + access + "; name=" + name
				+ "; desc=" + desc + "; signature=" + signature
				+ "; exceptions=" + exceptions);

		// if we found the default constructor then ...
		if ("<init>".equals(name)) {

			LOG.debug("default constructor modifications.");
			// Replace interfaceType from constructor
			desc = changeInterfaceTypeDesc(desc);

			ConstructorModMethodVisitor mv = new ConstructorModMethodVisitor(
					super
							.visitMethod(access, name, desc, signature,
									exceptions), className, allInterfaceTypes);

			LOG.debug("<<<<< visitMethod - end. Constructor MethodVisitor="
					+ mv);

			return mv;

		}

		// else
		LOG.debug("<<<<< visitMethod - end. " + "methodName=" + name
				+ "; methodDescription=" + desc);
		return super.visitMethod(access, name, desc, signature, exceptions);
	}

	/**
	 * This Method change the type of parameter of the constructor
	 * 
	 * @param String
	 *            the descriptor that contains the parameter
	 * */

	private String changeInterfaceTypeDesc(String desc) {

		// Remove "(" and ";")
		// Extract param from desc => there are some param with the form
		int start = desc.indexOf("(");        
	    int end = desc.lastIndexOf(")");        

		String descStart = desc.substring(0, start + 1);
		String descEnd = desc.substring(end, desc.length());

		InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(
				desc);
		String params[] = parser.parse().toArray(new String[] {});
		String result = descStart;		

		for (int i = 0; i < params.length; i++) {
			boolean isArray = TypeUtils.isArray(params[i]);
			if (TypeUtils.isSearchedType(params[i], isArray,
					allInterfaceTypes, TypeUtils.INTERFACE) != null) {

				if (isArray) {
					result += Type
							.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference[].class);
				} else {
					result += Type
							.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
				}

			} else {
				result += params[i];
			}
		}
		result += descEnd;
		return result;

	}

	@Override
	public void visitEnd() {

		super.visitEnd();
	}

}
