 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.omg.CORBA.TCKind;

/**
 *
 * @author lacquaviva
 */
public class AnyTypeUtils extends TypeUtils {

    private static final TCKind ANY = TCKind.tk_any;

    @Override
    protected SearchedType processType(Class clsType) {

        
        return null;

    }

	/**
	 * replaceAnyTypesInMethodSignature
	 *  replace union types method signature with union type wrappers or java.lang.Object
	 * @param desc
	* @return
	 * @throws ClassGenerationException
	 */
	public static String replaceAnyTypesInMethodSignature(String desc) throws ClassGenerationException {

		String origReturnType = extractReturnType(desc);
		java.util.List<String> params = extractParameters(desc);

		int idx = origReturnType.lastIndexOf("[");
		String arrayReturnType = "";
		if (idx >= 0)
			arrayReturnType = origReturnType.substring(0, idx + 1);

		String returnType = origReturnType;
		if (origReturnType.length() > 2) {
			returnType = origReturnType.substring(idx + 2, origReturnType
					.length() - 1);
			returnType = Util.replaceSeparatorWithDot(returnType);

			boolean isAny = returnType.equals(AnyType.CORBA_ANY_TYPE);
			if (isAny) {
				String replaceType = "java/lang/Object";

				returnType = arrayReturnType + "L" + replaceType + ";";
			} else {
				returnType = origReturnType;
			}
		}

		List<String> paramsWithUnions = new ArrayList<String>();

		for (String origParamType : params) {
			String paramType = origParamType;
			// ignore holders which are with generic types
			if (origParamType.indexOf('>') < 0)
			{
				idx = origParamType.lastIndexOf("[");
				String arrayParamType = "";
				if (idx >= 0)
					arrayParamType = origParamType.substring(0, idx + 1);
				
				if (origParamType.length() > 2) {
					paramType = origParamType.substring(idx + 2);
					if (paramType.endsWith(";"))
						paramType = paramType.substring(0, paramType.length() - 1);
					paramType = Util.replaceSeparatorWithDot(paramType);
	
					boolean isAny = paramType.equals(AnyType.CORBA_ANY_TYPE);
					if (isAny) {
						String replaceType = "java/lang/Object";

						paramType = arrayParamType + "L" + replaceType + ";";
					} else {
						paramType = origParamType;
					}
				}
			}
			paramsWithUnions.add(paramType);
		}

		StringBuffer prms = new StringBuffer();
		for (String param : paramsWithUnions) {
			prms.append(param);
		}

		String newDesc = "(" + prms + ")" + returnType;

		return newDesc;
	}

	
	
	
	
    /**
     * isAnyType - verify that is union type
     * 
     * @param desc
     * @return
     */
    public static AnyType isAnyType(String desc, boolean isArray, Map<String, AnyType> allAnyTypes) {

        return (AnyType) TypeUtils.isSearchedType(desc, isArray, allAnyTypes, ANY);
    }

    @Override
    protected void setMethodTypes(MethodSignature methodSignature, Set<String> typeList) {
        methodSignature.setMethodAnyTypes(typeList);
    }


	@Override
	protected Map<String, Class> getFields(Class clsAny) {
		// TODO Auto-generated method stub
		return null;
	}
}
