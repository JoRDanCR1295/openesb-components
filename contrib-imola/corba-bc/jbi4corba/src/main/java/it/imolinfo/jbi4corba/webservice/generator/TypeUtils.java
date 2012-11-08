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

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;
import it.imolinfo.jbi4corba.webservice.runtime.CorbaTransformationUtils;
import it.imolinfo.jbi4corba.webservice.runtime.RuntimeInformation;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.InputStream;

@SuppressWarnings("unchecked")
public abstract class TypeUtils {

	public static final TCKind INTERFACE = TCKind.tk_objref;
	public static final TCKind UNION = TCKind.tk_union;
	public static final TCKind ANY = TCKind.tk_any;

	private static final Map PRIMITIVE_NAME_TYPE_MAP = new HashMap();
	private static final Set<String> PRIMITIVE_JAVA_PARAMETER_SIGN = new HashSet<String>();
	private static final Map<String, Class> PRIMITIVE_JAVA_LANG_TYPE_MAP = new HashMap<String, Class>();

    private static final Logger LOG
          = LoggerFactory.getLogger(TypeUtils.class);

	public static final String JAXB_XML_ELEMENTS = "Ljavax/xml/bind/annotation/XmlElements;";

	public static final String JAXB_XML_ELEMENT = "Ljavax/xml/bind/annotation/XmlElement;";

	public static final String JAXB_XML_TYPE = "Ljavax/xml/bind/annotation/XmlType;";

	/** Setup the primitives map. */
	static {
		PRIMITIVE_NAME_TYPE_MAP.put("boolean", Boolean.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("byte", Byte.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("char", Character.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("short", Short.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("int", Integer.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("long", Long.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("float", Float.TYPE);
		PRIMITIVE_NAME_TYPE_MAP.put("double", Double.TYPE);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("boolean", Boolean.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("byte", Byte.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("char", Character.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("short", Short.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("int", Integer.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("long", Long.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("float", Float.class);
		PRIMITIVE_JAVA_LANG_TYPE_MAP.put("double", Double.class);

		PRIMITIVE_JAVA_PARAMETER_SIGN.add("B");// byte
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("C");// char
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("D");// double
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("F");// float
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("I");// int
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("J");// long
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("S");// short
		PRIMITIVE_JAVA_PARAMETER_SIGN.add("Z");// boolean
	}

	/**
	 * search in methodSignatures return types and parameter types for union
	 * types
	 * 
	 * @param classesDir
	 * @param methodSignatures
	 * @throws it.imolinfo.jbi4corba.exception.ClassGenerationException
	 */
	public void processTypes(Set<Class> allIDLTypes, String classesDir,
			Map operationTypes, TCKind searchedType)
			throws ClassGenerationException {

		for (Class typeClass : allIDLTypes) {

			findType(typeClass, classesDir, operationTypes, searchedType);

		}

	}

	/**
	 * find union in a certain type and in its member classes recursively
	 * 
	 * @param typeName
	 * @param classesDir
	 * @param interfaceClassType
	 * @param isArray
	 * @param typeList
	 * @throws it.imolinfo.jbi4corba.exception.ClassGenerationException
	 */
	protected void findType(Class typeName, String classesDir,

	Map<String, SearchedType> operationTypes, TCKind searchedType)
			throws ClassGenerationException {

		if (isPrimitive(typeName.getName())) {
			return;
		}

		// if array remove brakes from the typeName
		// if (isArray) {
		// int idx = typeName.indexOf('[');
		// if (idx > 0)
		// typeName = typeName.substring(0, idx);
		// }

		// determine if the corba type is searchedType
		if (isSearchedType(typeName, classesDir, searchedType)) {

			if (typeName == null)
				return;

			// collect the uniontype needed info

			SearchedType st = processType(typeName);

			// add union types to the list for MethodSignature
			operationTypes.put(st.getTypeName(), st);
		}
	}

	/**
	 * determines if the certain type is searchedType
	 * 
	 * @param typeName
	 * @param classesDir
	 * @param interfaceClassType
	 * @return
	 * @throws ClassGenerationException
	 */
	private static boolean isSearchedType(Class typeClass, String classesDir,
			TCKind searchedType) throws ClassGenerationException {
		// load the Helper class to determine if the corba type is union
		
    	TypeCode typeC = getCorbaTypeTypeCode(typeClass, classesDir);
			if (typeC == null)
				return false;

			else
			if (typeC.kind().equals(searchedType))
				return true;
	

		return false;
	}

	/**
	 * getCorbaTypeTypeCode
	 * 
	 * 
	 * @param typeClass
	 * @param classesDir
	 * @param searchedType
	 * @return
	 * @throws ClassGenerationException
	 */
	public static TypeCode getCorbaTypeTypeCode(Class typeClass,
			String classesDir)
			throws ClassGenerationException {
		Class typeHelperClass = Util.classLoad(classesDir, typeClass.getName()
				+ "Helper");

		if (typeHelperClass != null) {

			return (TypeCode) invokeMethod(typeHelperClass, "type");
		} else
			return null;
	}
	
	/**
	 * getCorbaTypeTypeCode (based on URLClassLoader)
	 * @param paramAny 
	 * @param origObj 
	 * @param typeClass
	 * @param classesDir
	 * @param searchedType
	 * 
	 * 
	 * @return
	 * @throws ClassGenerationException
	 */
	public static void encloseCorbaTypeInAny(Any paramAny,
			Object origObj, Class typeClass, RuntimeInformation runtimeInfo, Map<String, TypeDef>typeDefs)
			 {
		URLClassLoader classLoader = runtimeInfo.getCorbaClassLoader();
		Class typeHelperClass = null;
		try {
			String typeClassStr = typeClass.getName();
			try
			{
			typeHelperClass = classLoader.loadClass( typeClassStr
					+ "Helper");
			} catch (ClassNotFoundException e) {
				// case of value types
				
				if (typeClassStr.endsWith("Impl"))
				{
					typeClassStr = typeClassStr.substring(0, typeClassStr.length() - 4);
					typeHelperClass = classLoader.loadClass( typeClassStr
							+ "Helper");
				}
				
			}
		
		//  TypeDefs
		if ((typeDefs.containsKey(typeClassStr)) && (typeHelperClass != null)) {
			TypeDef typeDef = typeDefs.get(typeClassStr);			
			String aliasedClassName = typeDef.getAliasedClassName();
			if (LOG.isDebugEnabled()) {
				LOG.debug("typeDef:" + typeDef);
				LOG.debug("aliasedClassName: " + aliasedClassName);
				LOG.debug("typeHelperClass: " + typeHelperClass.getName());			
				LOG.debug("typeHelperClass:" + typeHelperClass + " with classloader: "  + typeHelperClass.getClassLoader());
			}
			Class aliasedClass = null;
			if (TypeUtils.isPrimitive(aliasedClassName)) {
				aliasedClass = typeDef.getAliasedClass();
			} else {
				// java.lang.String[]
				// To avoids a smix cl problem with 3.3.1
				aliasedClass = Class.forName(aliasedClassName, false, classLoader);
				// aliasedClass = classLoader.loadClass(aliasedClassName);
			}	
			if (LOG.isDebugEnabled()) {
				LOG.debug("aliasedClass: " + aliasedClass + " with classloader: "  + aliasedClass.getClassLoader());
				LOG.debug("origObj: " + origObj.getClass().getName() + " with classloader: "  + origObj.getClass().getClassLoader());
			}
			
			Object objectValue = null;
			try {
				objectValue = typeDef.getAliasedObject(origObj);
				if (aliasedClass.equals(java.lang.Object.class)) {
					// Handles the "TypeDef of any" case
					if (LOG.isDebugEnabled()) {
						LOG.debug("TypeDef of Any case");
					}
					aliasedClass = org.omg.CORBA.Any.class;
					Any myAny = runtimeInfo.getOrb().create_any();
					// We have to "transform" this any...
					CorbaTransformationUtils.transformToAny(objectValue, myAny, runtimeInfo);
					objectValue = myAny;
				}
			} catch (NoSuchFieldException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			} catch (InstantiationException e) {
				e.printStackTrace();
			}
			Method method = typeHelperClass.getMethod("insert", Any.class, aliasedClass);
			try {				
				// Object objectValue = typeDef.getAliasedObject(origObj);					
				method.invoke(null, paramAny, objectValue);
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			} 			
		} else if (typeHelperClass != null) {
			Method method = typeHelperClass.getMethod("insert", Any.class, classLoader.loadClass(origObj.getClass().getName()));
			try {
				method.invoke(null, paramAny, origObj);
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
			 
		}
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}catch (ClassNotFoundException e1) {
			e1.printStackTrace();
		}
	}
	
	

	/**
	 * extractCorbaTypeFromAny
	 * 
	 * @param paramAny
	 * @param typeClass
	 * @param classLoader
	 * @return
	 */
	public static Object extractCorbaTypeFromAny(Any paramAny, String typeClassStr, 
			URLClassLoader classLoader)

			 {
		try {
			if (LOG.isDebugEnabled()) {
				LOG.debug("To extract ANY of id: " + paramAny.type().id() +" for class: " + typeClassStr);
			}
		} catch (BadKind e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		Class typeHelperClass = null;
		try {
			if(!typeClassStr.endsWith("Helper")){
                            typeHelperClass = classLoader.loadClass( typeClassStr
					+ "Helper");
			} else {
				typeHelperClass = classLoader.loadClass( typeClassStr);
            }		
			if (typeHelperClass != null) {
			Method method = typeHelperClass.getMethod("extract", Any.class);
			try {
				Object obj = method.invoke(null, paramAny);
				
				return obj;
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
			 
		}
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	
	/**
	 * readCorbaTypeFromAnyInputStream
	 * 
	 * @param paramAny
	 * @param typeClass
	 * @param classLoader
	 * @return
	 */
	public static Object readCorbaTypeFromAnyInputStream(InputStream paramAny, String typeClass, URLClassLoader classLoader)
			 {
		
		Class typeHelperClass = null;
		try {
			if(!typeClass.endsWith("Helper")){
                        typeHelperClass = classLoader.loadClass( typeClass
					+ "Helper");
                        }else{
                         typeHelperClass = classLoader.loadClass( typeClass);
                        }                

		if (typeHelperClass != null) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Using helper for class: " + typeHelperClass);
			}
			Method method = typeHelperClass.getMethod("read", InputStream.class);
			try {
				Object obj = method.invoke(null, paramAny);
				
				return obj;
			} catch (IllegalArgumentException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			 
		}
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * @param desc
	 * @return
	 */
	public static SearchedType isSearchedType(String desc, boolean isArray,
			Map allSearchedTypes, TCKind searchType) {

		SearchedType st = null;

		String type = getTypeFromTypeDescription(desc);

		st = (SearchedType) allSearchedTypes.get(type);

		return st;
	}

	/**
	 * 
	 * @param desc
	 * @return
	 */
	public static String getTypeFromTypeDescription(String desc) {
		String type = desc;
		if (desc.startsWith("L") && desc.endsWith(";")) {
			type = desc.substring(1, desc.length() - 1);
		} else if (desc.startsWith("[")) {
			type = desc.substring(getArrayDimmentionAsPrefix(desc).length(),
					desc.length());
			if (type.startsWith("L") && type.endsWith(";")) {
				type = type.substring(1, type.length() - 1);
			}
		}
		type = Util.replaceSeparatorWithDot(type);

		return type;
	}

	/**
	 * isSearchedTypeAll
	 * 
	 * @param desc
	 * @param isArray
	 * @param allSearchedTypes
	 * @return
	 */
	public static SearchedType isSearchedTypeAll(String desc, boolean isArray,
			Map allSearchedTypes) {
		SearchedType searchedT = null;
		if ((desc.length() <= 1 && !isArray) || (desc.length() <= 2 && isArray))
			return null; // primitive types

		  searchedT = isSearchedType(desc, isArray, allSearchedTypes, INTERFACE);
		if (searchedT == null)
			searchedT = isSearchedType(desc, isArray, allSearchedTypes, UNION);

		if (searchedT == null)
			searchedT = isSearchedType(desc, isArray, allSearchedTypes, ANY);

		return searchedT;
	}

	/**
	 * getFieldType
	 * 
	 * @param field
	 * @param methods
	 * @return
	 */
	protected Class getFieldType(String field, Method[] methods) {

		// determine the union field type using XXX(type param) method where XXX
		// is the field name
		for (Method method : methods) {
			if (method.getName().equals(field)
					&& method.getParameterTypes().length == 1) {
				return method.getParameterTypes()[0];
			}
		}
		return null;
	}

	/**
	 * invokeMethod - invoke method using reflection
	 * 
	 * @param clss
	 * @param methodName
	 * @return
	 */
	private static Object invokeMethod(Class clss, String methodName, Class...methodParameters){
		try {

			Method method = clss.getMethod(methodName, methodParameters);
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
	 * getTypeClass
	 * 
	 * @param className
	 * @param classesDir
	 * @param interfaceClassType
	 * @return
	 * @throws it.imolinfo.jbi4corba.exception.ClassGenerationException
	 */
	protected static Class getTypeClass(String className, String classesDir,
			Class interfaceClassType) throws ClassGenerationException {
		Class typeClass = null;

		// Catch the primitive types:
		if (isPrimitive(className)) {
			typeClass = (Class) PRIMITIVE_NAME_TYPE_MAP.get(className);
		} else {
			// otherwise, try to load th class directly
			typeClass = Util.classLoadQuiet(classesDir, className);
		}

		// Try adding the interface package to the type.
		if (typeClass == null) {

			String classWithInterfacePackage = interfaceClassType.getPackage()
					.getName()
					+ "." + className;
			typeClass = Util.classLoadQuiet(classesDir,
					classWithInterfacePackage);
		}
		if (typeClass != null
				&& (typeClass.getName().startsWith("java.lang") || typeClass
						.getName().startsWith("java.io"))) {
			// LOG.debug("No type class found. Let's try adding the the
			// java.lang package");

			typeClass = null;
		}
		return typeClass;
	}

	/**
	 * 
	 * @param clsType
	 */
	protected abstract SearchedType processType(Class clsType);

	/**
	 * getUnionFields
	 * 
	 * @param clsUnion
	 * @return
	 */
	protected abstract Map<String, Class> getFields(Class clsUnion);

	/**
	 * 
	 * Add MethodType to methodSignature
	 */
	protected abstract void setMethodTypes(MethodSignature methodSignature,
			Set<String> typeList);

	/**
	 * isPrimitive
	 * 
	 * @param type
	 * @return
	 */
	public static boolean isPrimitive(final String type) {
		return PRIMITIVE_NAME_TYPE_MAP.containsKey(type);
	}

	/**
	 * isPrimitive
	 * 
	 * @param type
	 * @return
	 */
	public static boolean isPrimitiveParamSign(final String type) {
		String typeNoArray = type;
		int idx = -1;
		idx = type.lastIndexOf('[');
		if (idx >= 0) {
			typeNoArray = typeNoArray.substring(idx + 1);
		}
		return PRIMITIVE_JAVA_PARAMETER_SIGN.contains(typeNoArray);
	}

	/**
	 * getJavaLangClassPrimitive
	 * 
	 * @param type
	 * @return
	 */
	public static Class getJavaLangClassPrimitive(final String type) {
		return PRIMITIVE_JAVA_LANG_TYPE_MAP.get(type);
	}

	/**
	 * isJavaType
	 * 
	 * @param type
	 * @return
	 */
	public static boolean isJavaType(final String type) {
		if (type.startsWith("java.lang") || type.startsWith("java.io"))
			return true;

		return false;
	}

	/**
	 * getTypeName
	 * 
	 * @param type
	 * @return
	 */
	public static String getTypeName(Class type) {
		if (type.isArray()) {

			Class cl = type;
			int dimensions = 0;
			while (cl.isArray()) {
				dimensions++;
				cl = cl.getComponentType();
			}
			StringBuffer sb = new StringBuffer();
			sb.append(cl.getName());
			for (int i = 0; i < dimensions; i++) {
				sb.append("[]");
			}
			return sb.toString();

		}
		return type.getName();
	}

	/**
	 * getTypeNameWithoutBrackets
	 * 
	 * @param type
	 * @return
	 */
	public static String getTypeNameWithoutBrackets(Class type) {
		String typeStr = getTypeName(type);
		int idx = typeStr.indexOf('[');
		if (idx > 0) {
			return typeStr.substring(0, idx);
		}
		return getTypeNameWithoutBrackets(typeStr);
	}
	
	/**
	 * getTypeNameWithoutBrackets
	 * 
	 * @param type
	 * @return
	 */
	public static String getTypeNameWithoutBrackets(String typeStr) {
		int idx = typeStr.indexOf('[');
		if (idx > 0) {
			return typeStr.substring(0, idx);
		}
		return typeStr;
	}	

	/**
	 * isArray - verify that a type is array
	 * 
	 * @param type
	 * @return
	 */
	public static boolean isArray(String type) {
		if (type.startsWith("[") && type.endsWith(";"))
			return true;
		return false;
	}

	/**
	 * getObjectDimensions return the dimensions of the respective obj array
	 * 
	 * @param obj
	 * @return
	 */
	public static int[] getObjectDimensions(Object obj) {
		int dimension = 0;
		int dimensions[];

		Class objClass = obj.getClass();

		while (objClass.isArray()) {
			dimension++;
			objClass = objClass.getComponentType();
		}

		dimensions = new int[dimension];
		for (int i = 0; i < dimension; i++) {
			dimensions[i] = 0;
		}
		int idxArr = 0;
		Object tempObj = obj;
		while (idxArr < dimension) {
			dimensions[idxArr] = Array.getLength(tempObj);
            LOG.debug("--> Arrays "+Arrays.toString((Object[])tempObj));
            LOG.debug("--> Arrays "+Arrays.toString(dimensions));
			if (dimensions[idxArr] == 0)
				break;
			tempObj = Array.get(tempObj, 0);
			idxArr++;
		}

		return dimensions;
	}

	/**
	 * extractParameters
	 * 
	 * @param desc
	 * @return
	 */
	protected static List<String> extractParameters(String desc) {

		List<String> listOfParams = new ArrayList<String>();
		String params = desc.substring(1, desc.lastIndexOf(")"));

		int index = 0;
		StringBuffer strBuff = new StringBuffer();
		while (index < params.length()) {
			int ch = params.charAt(index);

			switch (ch) {
			case 'B':
			case 'C':
			case 'D':
			case 'F':
			case 'I':
			case 'J':
			case 'S':
			case 'Z': {
				strBuff.append(params.charAt(index));
				listOfParams.add(strBuff.toString());
				strBuff = new StringBuffer();
				++index;
				break;
			}
			case 'L': {
				int semicol = params.indexOf(';', index + 1);
				// verify generics
				if ((params.length() > semicol + 2)
						&& (params.charAt(semicol + 1) == '>')) {
					semicol += 2;
				}
				strBuff.append(params.substring(index, semicol + 1));
				listOfParams.add(strBuff.toString());
				strBuff = new StringBuffer();
				index = semicol + 1;
				break;
			}
			case '[': {
				strBuff.append('[');
				++index;
				break;
			}
			}

		}

		return listOfParams;

	}

	/**
	 * extractReturnType
	 * 
	 * @param desc
	 * @return
	 */
	protected static String extractReturnType(String desc) {

		String returnType = desc.substring(desc.indexOf(")") + 1);

		return returnType;
	}

	/**
	 * getArrayDimmentionAsPrefix
	 * 
	 * @param cls
	 * @return
	 */
	public static String getArrayDimmentionAsPrefix(Class cls) {
		StringBuffer arrStr = new StringBuffer();
		while (cls.isArray()) {
			arrStr.append("[");
			cls = cls.getComponentType();
		}

		return arrStr.toString();
	}

	/**
	 * getArrayDimmentionAsPrefix
	 * 
	 * @param desc
	 * @return
	 */
	public static String getArrayDimmentionAsPrefix(String desc) {
		if (desc.startsWith("[")) {
			return desc.substring(0, desc.lastIndexOf("[") + 1);
		} else
			return "";
	}

	/**
	 * isEnumType
	 * 
	 * @param type
	 * @param enumSet
	 * @return
	 */
	public static boolean isEnumType(Class type, Set<String> enumSet) {

		String simpleType = getTypeNameWithoutBrackets(type);

		if (enumSet.contains(simpleType))
			return true;

		return false;
	}

	/**
	 * extractTypeFromTypeCode
	 *  extract the type of the typeCode object
	 * @param tc
	 * @return
	 */
	public static String extractTypeFromTypeCode(TypeCode tc)
	{
            // Due to a idlj bug (or so it seems), we have to support TWO dofferent
            // types of id:
            // "IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplex:1.0";            
            // "IDL:it/imolinfo/jbi4corba/test/testprovidercomplex/EchoComplex/EchoComplex:1.0";
            
		try {
			String tempType = tc.id();                     
                        String className = tc.name();                      
			int idxStart = tempType.indexOf(':');
			int idxEnd = tempType.lastIndexOf('/');
			if (idxEnd < 0)
			{
                            idxEnd = tempType.lastIndexOf('\\');
			}
			tempType = tempType.substring(idxStart + 1, idxEnd);
                        // For the "first" type of id
			if (!tempType.contains(className)) {
                            tempType = tempType + '/' + className;
                        }
                        
                        return Util.replaceSeparatorWithDot(tempType);
		} catch (BadKind e) {
			return null;
		}
	}
        
	

      
}
