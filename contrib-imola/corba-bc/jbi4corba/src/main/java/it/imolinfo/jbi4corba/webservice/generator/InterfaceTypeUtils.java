 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.generator.bcm.InternalMethodDescriptionParser;

import java.util.HashMap;
import java.util.Map;

import java.util.Set;
import org.objectweb.asm.Type;
import org.omg.CORBA.TCKind;

/**
 * 
 * @author Luca
 */
public class InterfaceTypeUtils extends TypeUtils {

	private static final TCKind INTERFACE = TCKind.tk_objref;

	private static final Logger LOG = LoggerFactory
			.getLogger(InterfaceTypeUtils.class);

	@Override
    protected SearchedType processType(Class clsType) {

		InterfaceType it = new InterfaceType(clsType.getName());

		return it;

	}

	@Override
	protected Map<String, Class> getFields(Class clsUnion) {
		return new HashMap<String, Class>();
	}

	/**
	 * isUnionType - verify that is union type
	 * 
	 * @param desc
	 * @return
	 */
	public static InterfaceType isInterfaceType(String desc, boolean isArray,
			Map<String, InterfaceType> allInterfaceTypes) {

		return (InterfaceType) TypeUtils.isSearchedType(desc, isArray,
				allInterfaceTypes, INTERFACE);
	}

	/**
	 * This Method change the type paramters and the return parameters
	 * 
	 * @param String
	 *            the descriptor that contains the parameter
	 * @param Map
	 *            th map that contains all the interfecs type
	 * */

	public static String changeInterfaceTypeDesc(String desc,
			Map allInterfaceTypes) {

		LOG.debug("Changing description: " + desc);		
		// Remove "(" and ";")
		// Extract param from desc => there are some param with the form
		int start = desc.indexOf("(");		
		String descStart = desc.substring(0, start + 1);		

		// Extract Params
		InternalMethodDescriptionParser parser = new InternalMethodDescriptionParser(desc);
		String params[] = parser.parse().toArray(new String[] {});

		// String params[] = Util.extractParamsformDesc(desc);
		String result = descStart;
		String retType = null;

		// Process All Parameters
		for (int i = 0; i < params.length; i++) {
			LOG.debug("Investigating: " + params[i]);
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

		result += ")";	
		
		retType = parser.getMethodDescriptionTail();
		// is ReturnType
		if (retType.contains("L")) {
					
			boolean isArray = TypeUtils.isArray(retType);
			if (retType != null) {

				if (TypeUtils.isSearchedType(retType, isArray,
						allInterfaceTypes, TypeUtils.INTERFACE) != null) {

					if (isArray) {

						retType = Type
								.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference[].class);
					} else {
						retType = Type
								.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
					}
				}
			}
		} if (retType != null) {
			result += retType;
		}		
		LOG.debug("Changing description end, returning: " + result);
		return result;

	}

	@Override
	protected void setMethodTypes(MethodSignature methodSignature,
			Set<String> typeList) {
		methodSignature.setMethodInterfaceTypes(typeList);
	}

}
