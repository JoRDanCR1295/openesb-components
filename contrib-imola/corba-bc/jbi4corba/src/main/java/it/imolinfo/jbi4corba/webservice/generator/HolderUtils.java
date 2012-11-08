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
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility class for holder parameters management.
 * @author marco
 *
 */
public class HolderUtils {
    
    /** Primitive type name -> class map. */
    private static final Map<String,Class> PRIMITIVE_NAME_TYPE_MAP = new HashMap<String,Class>();

    /** Setup the primitives map. */
    
    static
    {
       PRIMITIVE_NAME_TYPE_MAP.put("boolean", Boolean.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("byte", Byte.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("char", Character.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("short", Short.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("int", Integer.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("long", Long.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("float", Float.TYPE);
       PRIMITIVE_NAME_TYPE_MAP.put("double", Double.TYPE);
    }
    
    private static final Map<String,Class> PRIMITIVE_NAME_CLASS_MAP = new HashMap<String,Class>();

    /** Setup the primitives map. */
    /**FIX for the correct generation of the wsdl the primitive are replaced with Class**/
    static
    {
    	PRIMITIVE_NAME_CLASS_MAP.put("boolean", Boolean.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("byte", Byte.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("char", Character.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("short", Short.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("int", Integer.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("long", Long.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("float", Float.class);
    	PRIMITIVE_NAME_CLASS_MAP.put("double", Double.class);
    }
    
    // interface replacement descriptor type 
    // private static final String W3CEPR = org.objectweb.asm.Type.getDescriptor(javax.xml.ws.wsaddressing.W3CEndpointReference.class);
    private static final String W3CEPR = "javax.xml.ws.wsaddressing.W3CEndpointReference";
    
    /**
     * Logger.
     */
    private static final Logger LOG
      = LoggerFactory.getLogger(HolderUtils.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(HolderUtils.class);

    
    /**
     * Calculate the Corba Holder value type class and set it in the Param of the <code>MethodSignature</code>.
     * Gets also the interface class type.
     * @param classesDir
     * @param methodSignatures
     * @param interfaceTypes 
     * @param unionTypes 
     * @throws ClassGenerationException
     */
    public static void populateCorbaHolderValueType(String classesDir, List<MethodSignature> methodSignatures, Set<String> unionTypes, Set<String> interfaceTypes) 
        throws ClassGenerationException {      
            	
        for (MethodSignature methodSignature: methodSignatures) {
            String interfaceClassTypeName = methodSignature.getClassName();
            Class interfaceClassType = Util.classLoad(classesDir, interfaceClassTypeName, true);
            methodSignature.setClassType(interfaceClassType);
                                	
            for (Param param: methodSignature.getParameters()) {              
                try {
                    Class typeClass = getTypeClass(param.getTypeName(), classesDir, interfaceClassType, param.isArray());
                    param.setType(typeClass);
                    param.setTypeName(typeClass.getName());
                    // if the class is an holder, gets the value
                    if (param.isHolder()) {                        
                        // Gets the class value
                        Field value = typeClass.getField("value");                        
                        // sets the holder data in the object 
                        param.setHolderType(typeClass);     
                        
                        if(isPrimitive(value.getType().getName())){
                        	param.setPrimitive(true);
                        	param.setHolderValueType((Class)PRIMITIVE_NAME_CLASS_MAP.get(value.getType().getName()));
                        	
                        }else{
                        	
                        	param.setHolderValueType(value.getType());
                        }
                          
                      
                    }                                       
                    
                } catch (SecurityException e) {
                	String msg=MESSAGES.getString("CRB000558_Error_in_holder_inspection");
                    LOG.error(msg,e);
                    throw new ClassGenerationException(msg,e);

                } catch (NoSuchFieldException e) {
                	String msg=MESSAGES.getString("CRB000558_Error_in_holder_inspection");
                    LOG.error(msg,e);
                    throw new ClassGenerationException(msg,e);
                }catch (IllegalArgumentException e) {
                	String msg=MESSAGES.getString("CRB000558_Error_in_holder_inspection");
                    LOG.error(msg,e);
                    throw new ClassGenerationException(msg,e);
                }                    
            }
            
            // Gets the interface methods
            Method[] methods = interfaceClassType.getMethods();
            
            // look for the method
            for(int i = 0; i < methods.length; i++) {
                if (findMethod(methods[i], methodSignature, true, unionTypes, interfaceTypes)) {
                    methodSignature.setMethod(methods[i]);
                }
            }           
        }
    }
    
    /**
     * True if the <code>MethodSignature</code> is the same as the
     * <code>Method</code>. If the parameter is an Holder, two beahviour are implemented: <br/>
     * <li/> if <code>compareHolderTypes == true the comparison is with the <code>Param</code> holderType class name.
     * <li/> if <code>compareHolderTypes == false </code> the comparison is with the <code>Param</code>type class name. 
     *  
     * @param method
     * @param methodSignature
     * @param compareHolderTypes
     * @param setOfInterfaces 
     * @param setOfUnions 
     * 
     * @return
     */
    private static boolean findMethod(Method method, MethodSignature methodSignature, boolean compareHolderTypes, Set<String> setOfUnions, Set<String> setOfInterfaces) {
      
        if(!method.getName().equals(methodSignature.getMethodName())) {
            // False if the name is different
            return false;
        } else if (!(method.getParameterTypes().length == methodSignature.getParameters().size())) {
            // False if the length param is different 
            return false;
        }             		    		
        for (int i = 0; i < method.getParameterTypes().length; i++) {
           
        	        	
            Type parameterType = (Type)method.getGenericParameterTypes()[i];                        
            	
            String paramTypeFromMethodAsString = "";
            if (parameterType instanceof Class) {
                paramTypeFromMethodAsString = getTypeName((Class)parameterType);    
            } else {            		          
              	paramTypeFromMethodAsString = parameterType.toString();		            	           	
            }       
            Param param = methodSignature.getParameters().get(i);
            
            // If is not an holder or i do not care about holder comparison... 
            if (!param.isHolder() || !compareHolderTypes) {
                // Normal parameter                

                String paramTypeStr = param.getTypeName();
                
                // union type
                boolean isSearchedType = false;
                String paramTypeModified = null;               
                if (setOfUnions.contains(paramTypeStr) && !compareHolderTypes)
                {
                	isSearchedType = true;
                	paramTypeModified = param.getTypeName() + "Wrapper";               	
                }
                              
                if (setOfInterfaces.contains(paramTypeStr)  && !compareHolderTypes)
                {
                	isSearchedType = true;
                	paramTypeModified =  W3CEPR;              	
                }

                if (paramTypeStr.equals(AnyType.CORBA_ANY_TYPE)  && !compareHolderTypes)
                {
                	isSearchedType = true;
                	paramTypeModified = "java.lang.Object";               	
                }

                // If an array, the [] must be added
                if (param.isArray()) {
                    // Adds the array "[]"
                    for (int j = 0; j < param.getArrayDimension(); j++) {
                        paramTypeStr = paramTypeStr + "[]";
                        if (!compareHolderTypes)
                        	paramTypeModified = paramTypeModified + "[]";

                    }
                }
                if (!paramTypeStr.equals(paramTypeFromMethodAsString) && !isSearchedType && !param.isHolder()) {
                
                    return false;
               }
                
                
                if(paramTypeModified!=null){
                
                    if (!paramTypeModified.equals(paramTypeFromMethodAsString) && isSearchedType) {
                	
                        return false;
                    }
                }
                // for SearchedTypes we should look also at holders
                if (param.isHolder())
                {
                	String holderType =TypeUtils.getTypeNameWithoutBrackets(param.getHolderValueType());
                	StringBuffer holderWithSearchedType = new StringBuffer("javax.xml.ws.Holder<");
                	isSearchedType = false;
                	if (setOfUnions.contains(holderType))
                    {
                		isSearchedType = true;
                		holderWithSearchedType.append(holderType);
                		holderWithSearchedType.append("Wrapper");
                		
                    }
               	if (setOfInterfaces.contains(holderType))
                   {
               		isSearchedType = true;
               		holderWithSearchedType.append(W3CEPR);
               		
                   }
               	if (holderType.equals(AnyType.CORBA_ANY_TYPE))
                {
                	isSearchedType = true;
                	holderWithSearchedType.append("java.lang.Object");
                }
                	holderWithSearchedType.append(">");
                	
                	if (!holderWithSearchedType.toString().equals(paramTypeFromMethodAsString) && isSearchedType) {
                    	
                        return false;
                   }
                }
                

            } else {                              
                // Holder       
            	
                if (!param.getHolderType().getCanonicalName().equals(paramTypeFromMethodAsString)) {
                    return false;
                }
            }
        }   
        return true;
    }
    
    /**
     * If the method contains holder parameters, the signature can be changed, so try
     * to conect the <MethodSignature> with the  <code>Method</code> object.
     * @param method
     * @param methodSignature
     * @param classesDir
     * @param interfaceTypes 
     * @param unionTypes 
     * @return
     * @throws ClassGenerationException 
     */
    public static void populateChangedMethod(MethodSignature methodSignature, String classesDir, Set<String> unionTypes, Set<String> interfaceTypes) throws ClassGenerationException {

        String className = methodSignature.getClassName();
        Class interfaceClassType = Util.classLoad(classesDir, className);

        // Gets the interface methods
        Method[] methods = interfaceClassType.getMethods();

        // look for the method
            
        for(int i = 0; i < methods.length; i++) {
        	        		        	
            if (findMethod(methods[i], methodSignature, false, unionTypes, interfaceTypes)) {            	
                methodSignature.setChangedMethod(methods[i]);             
                // TODO: add break when the method was found....
            }
        }
    }
    
   
    /**
     * Try to load the class from the className.
     * First of all, try to load the class direclty. If no class is found, try to add the 
     * interface package name. If no class is further found, try to add the "java.lang" default package 
     * @param className
     * @param classesDir
     * @param interfaceClassType
     * @return
     * @throws ClassGenerationException 
     */
    private static Class getTypeClass(String className, String classesDir, Class interfaceClassType, boolean isArray) throws ClassGenerationException {
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
            LOG.debug("No type class found. Let's try adding the interface class package");
             String classWithInterfacePackage=null;
             if(interfaceClassType.getPackage()!=null){
            	 classWithInterfacePackage = interfaceClassType.getPackage().getName() + "." + className;
            }else{
            	 classWithInterfacePackage = interfaceClassType.getName() + "." + className;
            }
            typeClass = Util.classLoadQuiet(classesDir, classWithInterfacePackage);            
        }

        // Try adding the java.lang package to the type.
        if (typeClass == null) {
            LOG.debug("No type class found. Let's try adding the the java.lang package");
            String classWithJavaLangPackage = "java.lang." +  className;    
            typeClass = Util.classLoadQuiet(classesDir, classWithJavaLangPackage);
            typeClass = Util.classLoadQuiet(classesDir, classWithJavaLangPackage);
        }

        // If no class is found, throws an exception
        if (typeClass == null) {
        	String msg=MESSAGES.getString("CRB000559_Error_in_getting_the_parameter_class", 
        			new Object[] {className});
            LOG.error(msg);
            throw new ClassGenerationException(msg);


        }
        return typeClass;
    }
    
    /**
       * True if the type is a primitive type.
       * @param type
       * @return
       */
      public static boolean isPrimitive(final String type)
      {
          return PRIMITIVE_NAME_TYPE_MAP.containsKey(type);
      }

      /**
       * Return the class type name as string if the type is an array.
       * @param type
       * @return
       */
      static String getTypeName(Class type) {
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

}
