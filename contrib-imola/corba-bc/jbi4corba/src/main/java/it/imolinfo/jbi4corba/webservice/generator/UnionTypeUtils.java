 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

/**
 * UnionTypeUtils - class that searches for union types in corba's method
 * signatures
 * 
 * @author laurlg
 */
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.Set;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

public class UnionTypeUtils extends TypeUtils{

	

	public static final String UNION_WRAPPER_FIELD = "choiceValue";

	

	/**
	 * 
	 * @param clsType
	 */
        @Override
	protected  SearchedType processType(Class clsType) {
		// determine the discriminator type
		Class discrimType = getDiscriminatorType(clsType);
		// determine the union fields and their types
		Map<String, Class> classFields = getFields(clsType);
		UnionType ut = new UnionType(clsType.getName(), discrimType,
                    classFields);

	return ut;
	}
        
  

	/**
	 * getUnionFields
	 * 
	 * @param clsUnion
	 * @return
	 */
	protected Map<String, Class> getFields(Class clsUnion) {

		Method[] methods = clsUnion.getDeclaredMethods();
		Map<String, Class> fields = new HashMap<String, Class>();

		// obtain the union fields name: XXX field name given by verifyXXX
		// method
		for (Method method : methods) {
			String methodName = method.getName();
			if (methodName.startsWith("verify") && methodName.length() > 6) {
				String field = method.getName().substring(6);
				// obtain the types of the union fields
				Class fieldType = getFieldType(field, methods);
				fields.put(field, fieldType);
			}
		}

		return fields;
	}

	

	/**
	 * getDiscriminatorType
	 * 
	 * @param clsUnion
	 * @return
	 */
	private static Class getDiscriminatorType(Class clsUnion) {

		try {

			Method discrMethod = clsUnion.getMethod("discriminator");

			return discrMethod.getReturnType();

		} catch (SecurityException e) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - getDiscriminatorType: SecurityException - discriminator",
					e);
		} catch (NoSuchMethodException e) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - getDiscriminatorType: NoSuchMethodException - discriminator",
					e);
		}
	}

	/**
	 * invokeMethod - invoke method using reflection
	 * 
	 * @param clss
	 * @param methodName
	 * @return
	 */
	private static Object invokeMethod(Class clss, String methodName) {
		try {

			Method method = clss.getMethod(methodName);
			return method.invoke(clss);

		} catch (NoSuchMethodException ex) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - invokeMethod: NoSuchMethodException - "
							+ methodName, ex);
		} catch (SecurityException ex) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - invokeMethod: SecurityException - "
							+ methodName, ex);
		} catch (IllegalAccessException ex) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - invokeMethod: IllegalAccessException - "
							+ methodName, ex);
		} catch (IllegalArgumentException ex) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - invokeMethod: IllegalArgumentException - "
							+ methodName, ex);
		} catch (InvocationTargetException ex) {
			throw new Jbi4CorbaRuntimeException(
					"UnionTypeUtils - invokeMethod: InvocationTargetException - "
							+ methodName, ex);
		}
	}

	
	/**
	 * createUnionClassWrapper - create a wrapper for the union types as parameter/return type
	 * 
	 * @param union
	 * @param allUnionTypes
	 * @param workdirclasses
	 * @return
	 * @throws ClassGenerationException
	 */
	public static String createUnionClassWrapper(UnionType union, Map<String, UnionType> allUnionTypes, String workdirclasses) throws ClassGenerationException {
		ClassWriter cw = new ClassWriter(false);
		FieldVisitor fv;
		MethodVisitor mv;
		AnnotationVisitor av0;
		String classWithPackage = union.getTypeName();
		String className = classWithPackage.substring(classWithPackage.lastIndexOf(".") + 1);

		classWithPackage = classWithPackage.replace('.', '/') + "Wrapper";

		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER,
				classWithPackage, null, "java/lang/Object", null);

		cw.visitSource(className + "Wrapper.java", null);

		{
			av0 = cw.visitAnnotation(JAXB_XML_TYPE,
					true);
			av0.visit("name", className);
			av0.visitEnd();
		}
		{
			fv = cw.visitField(0, UNION_WRAPPER_FIELD, "Ljava/lang/Object;", null,
					null);
			addUnionTypeAnnotation(fv, union, allUnionTypes);
			fv.visitEnd();
		}
		{
			mv = cw
					.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null,
							null);
			mv.visitCode();
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object",
					"<init>", "()V");
			mv.visitInsn(Opcodes.RETURN);
			mv.visitMaxs(1, 1);
			mv.visitEnd();
		}
		cw.visitEnd();
		
		String absPath = workdirclasses + File.separator
	      + union.getTypeName().replace('.', File.separatorChar) + "Wrapper.class";
		
		byte [] newBytecode = cw.toByteArray();

	    Util.saveAsJavaClass(absPath, newBytecode);
		
		return classWithPackage;
	}
	
	
	/**
	 * isUnionType - verify that is union type
	 * 
	 * @param desc
	 * @return
	 */
	public static UnionType isUnionType(String desc, boolean isArray, Map<String, UnionType> allUnionTypes) {

		
		return (UnionType) TypeUtils.isSearchedType(desc, isArray, allUnionTypes, UNION);
	}

	

	/**
	 * addUnionTypeAnnotation
	 * 
	 * @param fieldVisitor
	 * @param union
	 * @param isArray
	 */
	public static void addUnionTypeAnnotation(FieldVisitor fieldVisitor,
			UnionType union, Map<String, UnionType> allUnionTypes) {

		AnnotationVisitor unionAnn = fieldVisitor.visitAnnotation(JAXB_XML_ELEMENTS, true);
		
		AnnotationVisitor xmlElems = unionAnn.visitArray("value");
		for (String field : union.getTypeFieldNameList()) {
			
			Class fieldClass = union.getFieldType(field);
			// MARCO (23/2): If no default is specified, this can be null
			if (fieldClass != null) {			    
			
    			String fieldTypeName = fieldClass.getName();
    			Type fieldType = Type.getType(fieldClass);
    			boolean isArrayField = fieldClass.isArray();
    			UnionType unionField = isUnionType(fieldTypeName, isArrayField, allUnionTypes);
    			String arrayStr = getArrayDimmentionAsPrefix(fieldClass);
    			boolean replaceWithObject = getTypeNameWithoutBrackets(fieldClass).equals(AnyType.CORBA_ANY_TYPE);
    			if (unionField != null)
    			{
    				fieldType = Type.getType(arrayStr + "L" + unionField.getTypeName().replace('.', '/') + "Wrapper" + ";");
    			}
    			if (replaceWithObject)
    			{
    				fieldType = Type.getType(arrayStr + "Ljava/lang/Object;");
    			}
    			
    			AnnotationVisitor xmlElem = xmlElems.visitAnnotation("", JAXB_XML_ELEMENT);
    			xmlElem.visit("type", fieldType);
    			xmlElem.visit("name",field);
    			xmlElem.visit("required", true);
    			xmlElem.visit("nillable", false);
    			xmlElem.visitEnd();
			}
			
		}
		xmlElems.visitEnd();
		unionAnn.visitEnd();
	}

	/**
	 * replaceUnionTypesInMethodSignature
	 *  replace union types method signature with union type wrappers or java.lang.Object
	 * @param desc
	 * @param allUnionTypes
	 * @param workingDirClasses
	 * @param replaceWithWrappers - if true replace with wrappers/if false replace with java.lang.Object
	 * @return
	 * @throws ClassGenerationException
	 */
	public static String replaceUnionTypesInMethodSignature(String desc,
			Map<String, UnionType> allUnionTypes, String workingDirClasses) throws ClassGenerationException {

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

			UnionType ut = allUnionTypes.get(returnType);
			if (ut != null) {
				String replaceType = ut.getTypeName().replace('.', '/') + "Wrapper";

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
	
					UnionType utParam = allUnionTypes.get(paramType);
					if (utParam != null) {
						String replaceType = utParam.getTypeName().replace('.', '/') + "Wrapper";
	
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

	


    @Override
    protected void setMethodTypes(MethodSignature methodSignature, Set<String> typeList) {
        methodSignature.setMethodUnionTypes(typeList);
    }

  
	
}
