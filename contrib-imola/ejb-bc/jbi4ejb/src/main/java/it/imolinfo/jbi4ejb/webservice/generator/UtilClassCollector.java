/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Provides the methods used to visit a class and collect all the data types
 * used as field, method parameter, exception and return type.
 */
final class UtilClassCollector {

    /**
     * Logger for the class.
     */
    private static final Logger LOG = LoggerFactory
            .getLogger(UtilClassCollector.class);
    
    
    /**
     * Instantiates a new util class collector.
     */
    private UtilClassCollector() {}

    /**
     * Provides the methods used to visit a class and collect all the data types
     * used as field, method parameter, exception and return type.
     *
     * @param        result       The container for the class collected.
     * @param        clazz        The new class to visit.
     *
     * @return    The updated class collection.
     */
    @SuppressWarnings("unchecked")
    static Set<Class> visitClassCollector(Set<Class> result, Class clazz) {

        Set<Class> returnedResult = result;
        
        if (skipClassColletor(result, clazz)) {

            LOG.debug("DON'T Collect[" + clazz + "]");

        } else {

            List<Class> types = null;

            // multi dimensional array ?
            if (clazz.isArray()) {
                LOG.debug("Found an array:" + clazz);

                returnedResult = visitClassCollector(result, clazz.getComponentType());

            } else {
                result.add(clazz);
                LOG.debug("Collect[" + clazz + "]");

                types = extractTypes(clazz);

                for (Class currentType : types) {
                    returnedResult = visitClassCollector(result, currentType);
                }
            }

        }

        return returnedResult;
    }

    /**
     * This method decides if a class must be visited or not.
     *
     * @param    result    The container for the class collection.
     * @param    clazz   The class to visit.
     *
     * @return    true: if clazz is null
     *                  or clazz is already visited
     *                  or clazz is in java.*, javax.*, org.omg.*
     *                  or clazz is a java primitives data types
     *                  or an array.
     */
    @SuppressWarnings("unchecked")
    protected static boolean skipClassColletor(Set<Class> result, Class clazz) {
        if ((clazz == null) 
                || (contains(result, clazz))){
            return true;
        }

        String name = clazz.getCanonicalName();
        // if java, javax, org.omg or primitive
        if ((name.startsWith("java.") || name.startsWith("javax.") ||
                name.startsWith("org.omg.")) 
                || (clazz.isPrimitive()) ) {
            return true;
        }
        
        return false;
    }

    /**
     * Return true if the class is contained in the set.
     * 
     * @param result
     *          The classes set
     * @param clazz
     *          The class to search for
     * 
     * @return true, if the class is contained in the set.
     */
    @SuppressWarnings("unchecked")
    protected static boolean contains(Set<Class> result, Class clazz) {
        if (result == null) {
            return false;
        }
        // else
        String className = null;
        if (clazz != null) {
            className = clazz.getCanonicalName();
        } 

        for (Class item : result) {
            String itemClassName = item.getCanonicalName();

            if (itemClassName.equals(className)) {
                return true;
            }
        }

        return false;
    }

    /**
     * This method extracts all the data types used in the class.
     * (Fields and parameter, Excecption, return for each method).
     *
     * @param    clazz    The class inspected.
     *
     * @return    The list of class cllected. (never null).
     */
    @SuppressWarnings("unchecked")
    public static List<Class> extractTypes(Class clazz) {
        List<Class> types = new ArrayList<Class>();

        //types = exctraxtTypesFromConstructor(clazz, types);
        types = extractTypesFromMethods(clazz, types);
        types = extractTypesFromFields(clazz, types);

        return types;
    }

    /**
     * This method extracts the type of each public field of the class.
     * (protected and private field are ignored.)
     *
     * @param        clazz    The class to visit.
     * @param        types    The data types already collected.
     *
     * @return    The data types collection updated.
     */
    @SuppressWarnings("unchecked")
    private static List<Class> extractTypesFromFields(Class clazz,
            List<Class> types) {

        Field[] fields = clazz.getFields();
        if (fields == null) {
            return types;
        }
        // else
        for (Field currField : fields) {
            Class ft = currField.getType();
            if (!types.contains(ft)) {
                types.add(ft);
            }
        }

        return types;
    }

    /**
     * Extract types from methods (parameters, exceptions and return types). 
     * The types are returned if present in the types list.
     * 
     * @param clazz
     *             The class to inspect
     * @param types
     *              The types list 
     * 
     * @return the list< class>
     */
    @SuppressWarnings("unchecked")
    private static List<Class> extractTypesFromMethods(Class clazz,
            List<Class> types) {

        Method[] methods = clazz.getMethods();
        if (methods == null) {
            return types;
        }

        // else
        List<Class> returnedTypes = types;
        for (Method currentMethod : methods) {
            // ===== PARAMS
            Class[] paramTypes = currentMethod.getParameterTypes();
            returnedTypes = extractFromArray(paramTypes, types);

            // ===== EXCEPTIONS
            Class[] exTypes = currentMethod.getExceptionTypes();
            returnedTypes = extractFromArray(exTypes, types);

            // ===== RETURNS
            Class ret = currentMethod.getReturnType();
            if ((ret != null) && (!types.contains(ret))) {
                returnedTypes.add(ret);
            }
        }

        return returnedTypes;
    }

    /**
     * Adds all the classes in the array to the List and returns it.
     * 
     * @param array
     *          The classes array
     * @param types
     *          The tpyes array
     * 
     * @return the list< class>
     */
    @SuppressWarnings("unchecked")
    private static List<Class> extractFromArray(Class[] array, List<Class> types) {

        if (array == null) {
            return types;
        }
        // else
        for (Class currentType : array) {
            if (!types.contains(currentType)) {
                types.add(currentType);
            }
        }
        return types;
    }

}
