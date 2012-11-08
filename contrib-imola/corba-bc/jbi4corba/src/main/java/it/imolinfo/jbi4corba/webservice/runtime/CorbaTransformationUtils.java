/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.runtime;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaSUManager;
import it.imolinfo.jbi4corba.jbi.endpoint.Jbi4CorbaEndpoint;
import it.imolinfo.jbi4corba.jbi.endpoint.ProviderEndpoint;
import it.imolinfo.jbi4corba.utils.HelperEPRUtils;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.generator.AnyType;
import it.imolinfo.jbi4corba.webservice.generator.AnyTypeWrapper;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.MethodSignature;
import it.imolinfo.jbi4corba.webservice.generator.Param;
import it.imolinfo.jbi4corba.webservice.generator.SearchedType;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.UnionType;
import it.imolinfo.jbi4corba.webservice.generator.UnionTypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDef;
import it.imolinfo.jbi4corba.webservice.generator.typedef.TypeDefUtil;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;
import javax.xml.ws.Holder;
import javax.xml.ws.wsaddressing.W3CEndpointReference;
import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.omg.CORBA.Any;
import org.omg.CORBA.AnySeqHelper;
import org.omg.CORBA.BooleanSeqHelper;
import org.omg.CORBA.CharSeqHelper;
import org.omg.CORBA.DoubleSeqHelper;
import org.omg.CORBA.FloatSeqHelper;
import org.omg.CORBA.LongLongSeqHelper;
import org.omg.CORBA.LongSeqHelper;
import org.omg.CORBA.ORB;
import org.omg.CORBA.OctetSeqHelper;
import org.omg.CORBA.ShortSeqHelper;
import org.omg.CORBA.StringSeqHelper;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.ULongLongSeqHelper;
import org.omg.CORBA.ULongSeqHelper;
import org.omg.CORBA.UShortSeqHelper;
import org.omg.CORBA.WCharSeqHelper;
import org.omg.CORBA.WStringSeqHelper;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.portable.InputStream;

/**
 * This class provides utility functions to transform objects from jax-ws
 * objects to Corba objects and vice-a-versa
 */
public class CorbaTransformationUtils {

    /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(CorbaTransformationUtils.class);
    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES = Messages.getMessages(CorbaTransformationUtils.class);

    /**
     * changeFromCorbaToServiceObjects - Transform Corba objects to jax-ws
     * objects in operation return type and exceptions
     *
     *
     * @param corbaObject
     * @param runtimeInfo
     * @param objectType
     * @return
     * @throws SecurityException
     * @throws IllegalArgumentException
     * @throws ClassNotFoundException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws InstantiationException
     * @throws NoSuchMethodException
     */
    public static Object changeFromCorbaToServiceObject(Object corbaObject,
            RuntimeInformation runtimeInfo, Class objectType)
            throws SecurityException, IllegalArgumentException,
            ClassNotFoundException, NoSuchFieldException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException, NoSuchMethodException {

        if (corbaObject == null) {
            return null;
        }

        Object crbObject = corbaObject;

        Object serviceObject = null;
        int dimensions[] = null;
        LOG.debug("ObjectType ==>" + objectType);

        if (TypeUtils.isPrimitive(objectType.getName()) || TypeUtils.isJavaType(objectType.getName()) || TypeUtils.isPrimitiveParamSign(objectType.getName())) {
            return corbaObject;
        }

        if (TypeUtils.isEnumType(objectType, runtimeInfo.getAllEnumTypes().keySet())) {
            return transformEnumType(corbaObject, runtimeInfo.getCorbaClassLoader(),
                    runtimeInfo.getServiceClassLoader());
        }

        boolean returnTypeIsArray = corbaObject.getClass().isArray();
        if (returnTypeIsArray) {
            dimensions = TypeUtils.getObjectDimensions(corbaObject);
            LOG.debug("ISARRAY" + corbaObject.getClass().getName());

        }

        LOG.debug(runtimeInfo.getAllCorbaTypes().toString());

        SearchedType sType = TypeUtils.isSearchedTypeAll(TypeUtils.getTypeNameWithoutBrackets(objectType), false, runtimeInfo.getAllCorbaTypes());
        if (sType == null) {
            if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(objectType))) {
                sType = new AnyType();
            }
        }
        if (sType != null) {
            // transform

            if (sType instanceof UnionType) {
                String utWrapper = sType.getTypeName() + "Wrapper";
                Class utWrapperClass = runtimeInfo.getServiceClassLoader().loadClass(utWrapper);
                if (returnTypeIsArray) {
                    serviceObject = Array.newInstance(utWrapperClass,
                            dimensions);
                } else {
                    serviceObject = utWrapperClass.newInstance();
                }
                transformFromCorbaType(serviceObject, sType, crbObject,
                        runtimeInfo, true, null, null);
            }
            if (sType instanceof InterfaceType) {

                serviceObject = objectToEPR(crbObject, runtimeInfo.getJbi4CorbaEndpoint(), objectType);
            }
            if (sType instanceof AnyType) {
                AnyTypeWrapper anyWr = new AnyTypeWrapper();
                transformFromCorbaType(anyWr, sType, crbObject, runtimeInfo,
                        true, null, null);
                serviceObject = anyWr.getValue();
            }
        } else {
            // recursive comparish
            if (returnTypeIsArray) {
                serviceObject = Array.newInstance(
                        runtimeInfo.getServiceClassLoader().loadClass(
                        TypeUtils.getTypeNameWithoutBrackets(objectType)),
                        dimensions);
            } else {
                serviceObject = runtimeInfo.getServiceClassLoader().loadClass(objectType.getName()).newInstance();
            }
            LOG.debug("Modified Object===>" + serviceObject.getClass());
            LOG.debug("Orig Object===>" + crbObject.getClass());
            LOG.debug("Modified Object===>" + serviceObject.getClass().getClassLoader());
            LOG.debug("Orig Object===>" + crbObject.getClass().getClassLoader());
            buildServiceObjectfromCorbaObject(serviceObject, crbObject, runtimeInfo);          
        }
        return serviceObject;
    }

    /**
     * recursiveInvokationAdapter - Transform jax-ws objects to Corba objects in
     * operation parameters
     *
     * @param serviceParams
     * @param runtimeInfo
     * @param methodSignature
     * @return
     * @throws ClassNotFoundException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     */
    public static Object[] changeFromServiceToCorbaObjects(Object[] serviceParams,
            RuntimeInformation runtimeInfo, MethodSignature methodSignature)
            throws ClassNotFoundException, InstantiationException,
            IllegalAccessException, SecurityException, NoSuchFieldException,
            IllegalArgumentException, InvocationTargetException,
            NoSuchMethodException {
        int idx = 0;
        Object[] corbaParams = new Object[serviceParams.length];
        for (Param paramSig : methodSignature.getParameters()) {

            LOG.debug("RecursiveInvokationAdapter Params ==>" + serviceParams.length);
            LOG.debug(" Paramsig ==>" + paramSig.toString());
            LOG.debug(" Paramsig ==>" + paramSig.getName());
            LOG.debug(" Paramsig ==>" + paramSig.getTypeName());
            LOG.debug(" OriginalClassLoader==>" + runtimeInfo.getCorbaClassLoader());
            LOG.debug("Params ClassLoader" + serviceParams.getClass().getClassLoader());            
            if (paramSig.isHolder()) {
                // holders are already treated
                corbaParams[idx] = serviceParams[idx];

            } else {
                corbaParams[idx] = changeFromServiceToCorbaObject(serviceParams[idx], runtimeInfo, paramSig.getType());
            }
            idx++;
        }

        return corbaParams;
    }

    /**
     * recursiveInvokationAdapter - Transform jax-ws objects to Corba objects in
     * operation parameters
     * @TODO: ADD A CORRECT AND MORE MEANINGFUL COMMENT!!!!!!!!!!
     *
     * @param corbaParams
     * @param runtimeInfo
     * @param methodSignature
     * @return
     * @throws ClassNotFoundException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     */
    public static Object[] changeFromCorbaToServiceObjects(Object[] corbaParams,
            RuntimeInformation runtimeInfo, MethodSignature methodSignature)
            throws ClassNotFoundException, InstantiationException,
            IllegalAccessException, SecurityException, NoSuchFieldException,
            IllegalArgumentException, InvocationTargetException,
            NoSuchMethodException {
        int idx = 0;
        Object[] serviceParams = new Object[corbaParams.length];
        for (Param paramSig : methodSignature.getParameters()) {

            Object serviceObject = null;
            int dimensions[] = null;

            LOG.debug("ObjectType ==>" + paramSig.getType());

            if (paramSig.isHolder()) {
                // holders are already treated
                serviceParams[idx] = corbaParams[idx];
            } else {
                serviceParams[idx] = changeFromCorbaToServiceObject(corbaParams[idx], runtimeInfo, paramSig.getType());
            }
            idx++;
        }

        return serviceParams;
    }

    /**
     * changeFromCorbaToServiceObjects - Transform Corba objects to jax-ws
     * objects in operation return type and exceptions
     *
     *
     * @param res
     * @param runtimeInfo
     * @param objectType
     * @return
     * @throws SecurityException
     * @throws IllegalArgumentException
     * @throws ClassNotFoundException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws InstantiationException
     * @throws NoSuchMethodException
     */
    public static Object changeFromServiceToCorbaObject(Object serviceObject,
            RuntimeInformation runtimeInfo, Class objectType)
            throws SecurityException, IllegalArgumentException,
            ClassNotFoundException, NoSuchFieldException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException, NoSuchMethodException {

        if (serviceObject == null) {
            return null;
        }

        if (TypeUtils.isPrimitive(objectType.getName()) || TypeUtils.isJavaType(objectType.getName()) || TypeUtils.isPrimitiveParamSign(objectType.getName())) {
            return serviceObject;
        } else if (TypeUtils.isEnumType(objectType, runtimeInfo.getAllEnumTypes().keySet())) {
            return transformEnumType(serviceObject, runtimeInfo.getServiceClassLoader(), runtimeInfo.getCorbaClassLoader());
        } else {


            Class corbaClass = null;
            if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(objectType))) {
                corbaClass = Any.class;
            } else {
                corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                        TypeUtils.getTypeNameWithoutBrackets(objectType));
            }
            if (LOG.isDebugEnabled()) {
                if (serviceObject == null) {
                    LOG.debug(" =========> SERVICE OBJECT NULL <========");
                }
            }
            
            // MARCO: eì qua il problema...invece di essere un array di Any
            // Deve essere un ANY con dentro un array!
            // ut alla fine di questo metodo è corretto (è un'ANY), mentre crbObject no.
            LOG.debug(" ====>" + serviceObject.getClass().getName());
            LOG.debug(" ====>" + serviceObject.getClass().getClassLoader());

            LOG.debug("OriginalClass ==>" + corbaClass.toString());
            LOG.debug("ModifiedClass ==>" + serviceObject.toString());

            Object crbObject = null;
            
            if (serviceObject.getClass().isArray()) {
                if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(objectType))) {
                	LOG.debug("IS ANY, we have to put an array inside an ANY");
                    // crbObject = new Any[(Array.getLength(serviceObject)).length];
                	crbObject = runtimeInfo.getOrb().create_any();
                } else {
                    int dimentions[] = TypeUtils.getObjectDimensions(serviceObject);
                    crbObject = Array.newInstance(corbaClass, dimentions);
                }
                
            } else {
                if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(objectType))) {
                    crbObject = runtimeInfo.getOrb().create_any();
                } else {
                    try {
                        crbObject = corbaClass.newInstance();
                    } catch (InstantiationException e) {
                        LOG.debug("errore istanziando corba object", e);
                    }
                }
            }
            
            LOG.debug("OrigiObject ==>" + crbObject);

            boolean isArray = objectType.isArray();
            LOG.debug("objectType.isArray() ==>" + isArray);
            LOG.debug("objectType.getName() ==>" + objectType.getName());
            
            SearchedType ut = TypeUtils.isSearchedTypeAll(objectType.getName(), isArray,
                    runtimeInfo.getAllCorbaTypes());
            
            LOG.debug("ut: ==>" + ut);
            if (ut == null) {
                if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(objectType))) {
                    ut = new AnyType();
                }
            }
            if (ut != null) {
                // transform
                LOG.debug("Transform=====>" + crbObject);
                LOG.debug("CLASSLOADER=====>" + serviceObject.getClass().getClassLoader());

                if (ut instanceof InterfaceType) {
                    String ior = null;
                    if (serviceObject != null && serviceObject instanceof W3CEndpointReference) {
                        W3CEndpointReference epr = (W3CEndpointReference) serviceObject;

                        LOG.debug("Document Fragment EPR ===> " + epr.toString());
                        ior = HelperEPRUtils.readAddressFromEPR(epr.toString());
                        LOG.debug("IOR ===> " + ior);

                    }

                    LOG.debug("IOR ===> " + ior);
                    // Load Helper and Do the narrow
                    // Class helperClass =
                    // originalClassLoader.loadClass(origFields[i].getType().getName()
                    // + "Helper");
                    if (!ior.equals("")) {
                        Class helperClass = loadHelperbyName(
                                corbaClass.getName(),
                                runtimeInfo.getCorbaClassLoader());
                        crbObject = narrowObjbyIOR(helperClass,
                                ior, runtimeInfo.getOrb());
                    }
                } else {
                    LOG.debug("ut before transformToCorbaType ==>" + ut);
                    transformToCorbaType(crbObject, ut, serviceObject,
                            runtimeInfo, true);
                }
            } else {
                // recursive comparish
                LOG.debug("Fill=====>" + crbObject);
                LOG.debug("CLASSLOADER=====>" + serviceObject.getClass().getClassLoader());

                if (TypeUtils.isPrimitive(crbObject.getClass().getName()) || TypeUtils.isJavaType(crbObject.getClass().getName()) || TypeUtils.isPrimitiveParamSign(crbObject.getClass().getName())) {
                    crbObject = serviceObject;
                } else if (TypeUtils.isEnumType(crbObject.getClass(),
                        runtimeInfo.getAllEnumTypes().keySet())) {
                    crbObject = transformEnumType(serviceObject, runtimeInfo.getServiceClassLoader(), runtimeInfo.getCorbaClassLoader());
                } else {
                    buildCorbaObjectfromServiceObject(crbObject, serviceObject,
                            runtimeInfo);
                }
            }
            return crbObject;
        }

    }

    /**
     * Change the Holder params for the correct servant invocation.
     *
     * @param methodSignature
     * @param params
     * @param runtimeInfo
     * @return
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws ClassNotFoundException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     */
    public static Object[] changeHoldersFromServiceToCorbaObjects(
            MethodSignature methodSignature, List<Object> params,
            RuntimeInformation runtimeInfo) throws InstantiationException,
            IllegalAccessException, SecurityException, NoSuchFieldException,
            ClassNotFoundException, IllegalArgumentException,
            InvocationTargetException, NoSuchMethodException {

        // This method :
        // - for each param, get the holder value (casting to the method real
        // type)
        // - Instanciate the CORBA Holder param
        // - Set the value into the CORBA Holder param
        // -Return the Object array for the CORBA invocation

        ArrayList<Object> corbaParams = new ArrayList<Object>();

        for (int i = 0; i < methodSignature.getParameters().size(); i++) {
            Param paramSig = (Param) methodSignature.getParameters().get(i);

            // If holer, the valuemust be copied.
            Object value = null;
            if (paramSig.isHolder()) {

                // Reads ths jaxws holder value
                Holder jaxwsHolder = (Holder) params.get(i);
                if (jaxwsHolder.value == null) {
                    LOG.debug("Holder is Null!!!!");
                    //LOG.debug("--> NULL AND ARRAY "+paramSig.getHolderValueType().isArray());
                    if (paramSig.getHolderValueType().isArray()) {
                        // LOG.debug("--> NULL AND ARRAY "+paramSig.getType().isArray());
                        value = Array.newInstance(paramSig.getType(), 0);
                    }
                } else {
                    value = jaxwsHolder.value;
                }
                Object corbaHolder = null;
                Object actualHolderValueObject = null;

                Class corbaHolderType = runtimeInfo.getCorbaClassLoader().loadClass(paramSig.getHolderType().getName());

                corbaHolder = corbaHolderType.newInstance();

                // Gets the class value field and sets it as accessible
                Field corbaHolderValueField = corbaHolderType.getField("value");
                corbaHolderValueField.setAccessible(true);

                if (TypeUtils.isPrimitive(paramSig.getHolderValueType().getName()) || TypeUtils.isJavaType(paramSig.getHolderValueType().getName()) || TypeUtils.isPrimitiveParamSign(paramSig.getHolderValueType().getName())) {
                    LOG.debug("Holder value type is PRIMITIVE or JAVA class");
                    actualHolderValueObject = value;
                } else if (TypeUtils.isEnumType(paramSig.getHolderValueType(),
                        runtimeInfo.getAllEnumTypes().keySet())) {
                    actualHolderValueObject = transformEnumType(value,
                            runtimeInfo.getServiceClassLoader(), runtimeInfo.getCorbaClassLoader());
                } else {
                    // check if holder has searchedType as value
                    String holderValueTypeStr = TypeUtils.getTypeNameWithoutBrackets(paramSig.getHolderValueType());
                    SearchedType sType = TypeUtils.isSearchedTypeAll(
                            holderValueTypeStr, false, runtimeInfo.getAllCorbaTypes());
                    if (AnyType.isAnyType(holderValueTypeStr)) {
                        sType = new AnyType();
                    }

//					LOG.debug("Read value: " + value + " of class: "
//							+ value.getClass().getName() + "ClassLoader:"
//							+ value.getClass().getClassLoader());
                    // Gets the corba holder class, loading it with the actual
                    // classLoader.
                    if (sType instanceof InterfaceType) {
                        String ior = null;
                        if (value instanceof W3CEndpointReference && value != null) {
                            W3CEndpointReference epr = (W3CEndpointReference) value;

                            LOG.debug("Document Fragment EPR ===> " + epr.toString());
                            ior = HelperEPRUtils.readAddressFromEPR(epr.toString());
                            LOG.debug("IOR ===> " + ior);

                        }

                        LOG.debug("IOR ===> " + ior);
                        // Load Helper and Do the narrow
                        // Class helperClass =
                        // originalClassLoader.loadClass(origFields[i].getType().getName()
                        // + "Helper");
                        if (!ior.equals("")) {
                            Class helperClass = loadHelperbyName(
                                    corbaHolderValueField.getType().getName(),
                                    runtimeInfo.getCorbaClassLoader());
                            actualHolderValueObject = narrowObjbyIOR(helperClass,
                                    ior, runtimeInfo.getOrb());
                        }

                    } else {

                        if (sType instanceof AnyType) {
                            actualHolderValueObject = runtimeInfo.getOrb().create_any();
                        } else {
                            Class origHolderValueClass = runtimeInfo.getCorbaClassLoader().loadClass(
                                    TypeUtils.getTypeNameWithoutBrackets(corbaHolderValueField.getType()));

                            if (value.getClass().isArray()) {

                                //Added to manage ampty array
                                if (value == null) {
                                    actualHolderValueObject = Array.newInstance(
                                            origHolderValueClass, 0);
                                    LOG.debug("!!!!!>>> Is Empty array: ");
                                } /*********************************************/
                                else {
                                    int dimentions[] = TypeUtils.getObjectDimensions(value);
                                    actualHolderValueObject = Array.newInstance(
                                            origHolderValueClass, dimentions);
                                    if (LOG.isDebugEnabled()) {
                                    	LOG.debug("!!!!!>>> Is array: " + Arrays.toString(dimentions));
                                    }
                                }

                            } else {
                                actualHolderValueObject = origHolderValueClass.newInstance();
                            }

                        }
                        // Gets ths actual holder, set it as the value and sets
                        // to the "value" corbaholder field.
                        LOG.debug("!!!!!!!!! >>>>>orig holder: " + actualHolderValueObject);
                        LOG.debug("!!!!!!!!! >>>>>mod holder: " + value);
                        if (sType != null) {
                            transformToCorbaType(actualHolderValueObject,
                                    sType, value, runtimeInfo, true);
                        } else {
                            if (TypeUtils.isPrimitive(actualHolderValueObject.getClass().getName()) || TypeUtils.isJavaType(actualHolderValueObject.getClass().getName()) || TypeUtils.isPrimitiveParamSign(actualHolderValueObject.getClass().getName())) {
                                actualHolderValueObject = value;
                            } else if (TypeUtils.isEnumType(
                                    actualHolderValueObject.getClass(),
                                    runtimeInfo.getAllEnumTypes().keySet())) {
                                actualHolderValueObject = transformEnumType(
                                        value, runtimeInfo.getServiceClassLoader(),
                                        runtimeInfo.getCorbaClassLoader());
                            } else {
                                buildCorbaObjectfromServiceObject(
                                        actualHolderValueObject, value,
                                        runtimeInfo);
                            }
                        }
                    }
                }
                corbaHolderValueField.set(corbaHolder, actualHolderValueObject);
                                               
                corbaParams.add(corbaHolder);

            } else {                
                corbaParams.add(params.get(i));
            }

        }
        return corbaParams.toArray();
    }

    /**
     * Change the Holder params after the servant invocation. It's the opposite
     * of method <code>changeHolderParamsBeforeServantInvocation</code>: get
     * the Corba holder values and set it into the
     * <code>javax.xml.ws.Holder</code> params elements.
     *
     *
     * @param methodSignature
     * @param params
     * @param paramArrayAfterInvocation
     * @param runtimeInfo
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws ClassNotFoundException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     */
    public static void changeHoldersFromCorbaToServiceObjects(
            MethodSignature methodSignature, List<Object> params,
            Object[] paramArrayAfterInvocation, RuntimeInformation runtimeInfo)
            throws InstantiationException, IllegalAccessException,
            SecurityException, NoSuchFieldException, IllegalArgumentException,
            ClassNotFoundException, InvocationTargetException,
            NoSuchMethodException {

        int idx = 0;
        for (int i = 0; i < methodSignature.getParameters().size(); i++) {
            Param paramSig = (Param) methodSignature.getParameters().get(i);
            // If holder, the value must be copied. If no holder, no action must
            // be processed.
            if (paramSig.isHolder()) {

                // check if holder has searchedType as value
                String holderValueTypeStr = TypeUtils.getTypeNameWithoutBrackets(paramSig.getHolderValueType());
                SearchedType sType = TypeUtils.isSearchedTypeAll(
                        holderValueTypeStr, false, runtimeInfo.getAllCorbaTypes());
                if (AnyType.isAnyType(holderValueTypeStr)) {
                    sType = new AnyType();
                }

                // Gets the actual value from the CORBA holder, for example the
                // String contained into the StringHolder.
                Field valueCorbaField = (paramArrayAfterInvocation[i]).getClass().getField("value");
                Object value = valueCorbaField.get(paramArrayAfterInvocation[i]);
                LOG.debug("corba holder value type: " + (value != null ? value.getClass() : "null value"));
                // Sets the value into the javax.xml.ws.Holder
                Holder holder = (Holder) params.get(i);
                // holder.value = value;

                //null value handling
                if (value == null) {
                    holder.value = null;
                    continue;
                }

                if (sType != null) {
                    if (sType instanceof AnyType) {
                        AnyTypeWrapper anyWr = new AnyTypeWrapper();
                        transformFromCorbaType(anyWr, sType, value,
                                runtimeInfo, true, null, null);
                        holder.value = anyWr.getValue();
                    } //Added by Luca for bug fixing with EPR
                    //when interface are used with holder rhe invocation return the request parameter
                    //Now it seems fixed
                    else if (sType instanceof InterfaceType) {

                        holder.value = objectToEPR(value, runtimeInfo.getJbi4CorbaEndpoint(), holder.getClass());
                    } else {
                        if (sType instanceof UnionType) {
                            Class srvClass = runtimeInfo.getServiceClassLoader().loadClass(
                                    holderValueTypeStr + "Wrapper");
                            holder.value = srvClass.newInstance();
                        }
                        transformFromCorbaType(holder.value, sType, value,
                                runtimeInfo, true, null, null);
                    }
                } else {
                    if (TypeUtils.isPrimitive(value.getClass().getName()) || TypeUtils.isJavaType(value.getClass().getName()) || TypeUtils.isPrimitiveParamSign(value.getClass().getName())) {
                        holder.value = value;
                    } else if (TypeUtils.isEnumType(value.getClass(),
                            runtimeInfo.getAllEnumTypes().keySet())) {
                        holder.value = transformEnumType(value, runtimeInfo.getCorbaClassLoader(), runtimeInfo.getServiceClassLoader());
                    } else {

                        if (value.getClass().isArray()) {
                            holder.value = Array.newInstance(
                                    runtimeInfo.getServiceClassLoader().loadClass(TypeUtils.getTypeNameWithoutBrackets(value.getClass())), TypeUtils.getObjectDimensions(value));
                        } else {
                            holder.value = runtimeInfo.getServiceClassLoader().loadClass(TypeUtils.getTypeNameWithoutBrackets(value.getClass())).newInstance();
                        }
                        buildServiceObjectfromCorbaObject(holder.value, value,
                                runtimeInfo);
                    }
                // actuaHolder = value;
                }
                LOG.debug("Holder After---->" + holder.value.getClass());
                LOG.debug("Holder After---->" + holder.value.toString());
                paramArrayAfterInvocation[idx] = holder;
            }
            idx++;
        }
    }

    /**
     * buildServiceObjectfromCorbaObject - Transform Corba objects to jax-ws objects in
     * parameter
     *
     * @param serviceObject
     * @param corbaObject
     * @param runtimeInfo
     * @throws ClassNotFoundException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws InstantiationException
     * @throws NoSuchMethodException
     */
    private static void buildServiceObjectfromCorbaObject(Object serviceObject,
            Object corbaObject, RuntimeInformation runtimeInfo)
            throws ClassNotFoundException, SecurityException,
            NoSuchFieldException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException,
            InstantiationException, NoSuchMethodException {

        LOG.debug("ORIG OBJECT ==>" + corbaObject);
        LOG.debug("MODIFIED OBJECT ==>" + serviceObject);

        if (serviceObject.getClass().isArray()) {
            LOG.debug("!!!!>>>>>> IsARRAY");
            Object[] corbaObjectArray = (Object[]) corbaObject;
            Object[] serviceObjectArray = (Object[]) serviceObject;
            LOG.debug("!!!!>>>>>> arrayLent: " + corbaObjectArray.length);
            LOG.debug("!!!!>>>>>> component type: " + corbaObject.getClass().getComponentType());
            for (int i = 0; i < corbaObjectArray.length; i++) {
                // Invertire Original con modified
                if (corbaObjectArray[i].getClass().isArray()) {
                    serviceObjectArray[i] = Array.newInstance(
                            runtimeInfo.getServiceClassLoader().loadClass(
                            TypeUtils.getTypeNameWithoutBrackets(serviceObject.getClass())),
                            TypeUtils.getObjectDimensions(corbaObjectArray[i]));
                } else if (TypeUtils.isEnumType(corbaObjectArray[i].getClass(),
                        runtimeInfo.getAllEnumTypes().keySet())) {

                    serviceObjectArray[i] = transformEnumType(corbaObjectArray[i], runtimeInfo.getCorbaClassLoader(),
                            runtimeInfo.getServiceClassLoader());

                } else {
                    serviceObjectArray[i] = serviceObject.getClass().getComponentType().newInstance();
                }
                // origObjectArray[i] =
                // origObject.getClass().getComponentType().newInstance();
                if (TypeUtils.isPrimitive(corbaObjectArray[i].getClass().getName()) || TypeUtils.isJavaType(corbaObjectArray[i].getClass().getName()) || TypeUtils.isPrimitiveParamSign(corbaObjectArray[i].getClass().getName())) {
                    serviceObjectArray[i] = corbaObjectArray[i];
                } else if (TypeUtils.isEnumType(corbaObjectArray[i].getClass(),
                        runtimeInfo.getAllEnumTypes().keySet())) {
                    serviceObjectArray[i] = transformEnumType(
                            corbaObjectArray[i], runtimeInfo.getCorbaClassLoader(), runtimeInfo.getServiceClassLoader());
                } else {
                    buildServiceObjectfromCorbaObject(serviceObjectArray[i],
                            corbaObjectArray[i], runtimeInfo);
                }
                LOG.debug("!!!!>>>>>> service component type object: " + serviceObjectArray[i]);
            }

        } else {

            Class corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                    TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
            Class serviceClass = runtimeInfo.getServiceClassLoader().loadClass(
                    TypeUtils.getTypeNameWithoutBrackets(serviceObject.getClass()));

            List<Field> allCrbFields = new ArrayList<Field>();
            getAllFields(corbaClass, allCrbFields);
            Field[] crbFields = allCrbFields.toArray(new Field[allCrbFields.size()]);
            LOG.debug("ORIGFIELDS LENGHT" + crbFields.length);
            LOG.debug("MODIFIED OBJECT" + serviceObject);

            LOG.debug("ORIG OBJECT" + corbaObject);
            LOG.debug("MODIFIED CLASS" + serviceClass);
            for (int i = 0; i < crbFields.length; i++) {
                crbFields[i].setAccessible(true);
                Field srvField = getField(serviceClass, crbFields[i].getName());// modClass.getDeclaredField(origFields[i].getName());
                srvField.setAccessible(true);
                LOG.debug("ORIG FIELD" + crbFields[i].getName());
                LOG.debug("MOD FIELD" + srvField.getName());
                LOG.debug("ORIG FIELD VALUE" + crbFields[i].get(corbaObject));

                if (srvField.getName().equals(crbFields[i].getName())) {
                    // field is primitive or java type
                    LOG.debug(">>>>>>>>>>>" + crbFields[i].getType().getName());
                    if (TypeUtils.isPrimitive(crbFields[i].getType().getName()) || TypeUtils.isJavaType(crbFields[i].getType().getName()) || TypeUtils.isPrimitiveParamSign(crbFields[i].getType().getName())) {

                        LOG.debug("PRIMITIVE");
                        srvField.set(serviceObject, crbFields[i].get(corbaObject));

                    } else if (TypeUtils.isEnumType(crbFields[i].getType(),
                            runtimeInfo.getAllEnumTypes().keySet())) {
                        srvField.set(serviceObject, transformEnumType(
                                crbFields[i].get(corbaObject), runtimeInfo.getCorbaClassLoader(), runtimeInfo.getServiceClassLoader()));
                    } // complex type
                    else {

                        Class crbFieldClass = crbFields[i].getType();
                        LOG.debug("FieldType====>" + crbFieldClass.getName());
                        SearchedType sType = TypeUtils.isSearchedTypeAll(
                                TypeUtils.getTypeNameWithoutBrackets(crbFieldClass),
                                false, runtimeInfo.getAllCorbaTypes());
                        if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(crbFieldClass))) {
                            sType = new AnyType();
                        }
                        if (sType != null) {
                            LOG.debug("CORBA ");
                            if (sType instanceof InterfaceType) {
                                 //5-01-2010 OSSSUPPORT 20 Fix
                                //After Fixing
                                //serviceObject = objectToEPR(crbFields[i].get(corbaObject), runtimeInfo.getJbi4CorbaEndpoint(), corbaClass);
                                //Managed intrefaceType on complexType
                                srvField.set(serviceObject, objectToEPR(crbFields[i].get(corbaObject), runtimeInfo.getJbi4CorbaEndpoint(), corbaClass));
                            } else {
                                Object srvFieldObj = null;
                                if (sType instanceof AnyType) {
                                    AnyTypeWrapper anyWr = new AnyTypeWrapper();

                                    transformFromCorbaType(anyWr, sType,
                                            crbFields[i].get(corbaObject),
                                            runtimeInfo, false, srvField,
                                            serviceObject);

                                    srvFieldObj = anyWr.getValue();
                                    srvField.set(serviceObject, srvFieldObj);
                                } else {
                                    if (crbFieldClass.isArray()) {
                                        srvFieldObj = Array.newInstance(
                                                runtimeInfo.getServiceClassLoader().loadClass(
                                                TypeUtils.getTypeNameWithoutBrackets(srvField.getType())),
                                                TypeUtils.getObjectDimensions(crbFields[i].get(corbaObject)));
                                    } else {
                                        srvFieldObj = runtimeInfo.getServiceClassLoader().loadClass(
                                                srvField.getType().getName()).newInstance();

                                    }
                                    LOG.debug("Union field: " + srvField.getName() + " -> " + srvFieldObj);
                                    transformFromCorbaType(srvFieldObj,
                                            sType, crbFields[i].get(corbaObject),
                                            runtimeInfo, false, srvField,
                                            serviceObject);
                                }
                            // modifField.set(modifiedObject,
                            // modifFieldObj);

                            }
                        } else {
                            // complex type but not corba type
                            LOG.debug("COMPLEX ");
                            LOG.debug(" TYPE==>" + crbFields[i].getType());
                            // ***

                            if (srvField.getType().getName().startsWith("[")) {
                                Object[] obj = (Object[]) crbFields[i].get(corbaObject);
                                LOG.debug("Modifield Fild Array Lenght" + obj.length);
                                srvField.set(serviceObject,
                                        Array.newInstance(srvField.getType().getComponentType(),
                                        obj.length));
                            } else {
                                srvField.set(serviceObject, srvField.getType().newInstance());
                            }
                            //LOG.debug(crbFields[i].toString());
                            if (crbFields[i].get(corbaObject) == null) {
                                continue;
                            }
                            if (TypeUtils.isPrimitive(crbFields[i].get(
                                    corbaObject).getClass().getName()) || TypeUtils.isJavaType(crbFields[i].get(
                                    corbaObject).getClass().getName()) || TypeUtils.isPrimitiveParamSign(crbFields[i].get(corbaObject).getClass().getName())) {
                                srvField.set(serviceObject, crbFields[i].get(corbaObject));
                            } else if (TypeUtils.isEnumType(crbFields[i].get(
                                    corbaObject).getClass(), runtimeInfo.getAllEnumTypes().keySet())) {
                                srvField.set(
                                        serviceObject,
                                        transformEnumType(
                                        crbFields[i].get(corbaObject),
                                        runtimeInfo.getCorbaClassLoader(),
                                        runtimeInfo.getServiceClassLoader()));
                            } else {
                                buildServiceObjectfromCorbaObject(srvField.get(serviceObject), crbFields[i].get(corbaObject), runtimeInfo);
                            }
                        }
                    }

                }
            }

        }
    }

    /**
     * transformFromCorbaType Transform Corba objects to jax-ws objects (corba
     * special types: Union, Interface, Any)
     *
     * @param serviceObject
     * @param sType
     * @param corbaObject
     * @param runtimeInfo
     * @param isParam
     * @param parentField
     * @param parentObject
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws ClassNotFoundException
     * @throws InvocationTargetException
     * @throws InstantiationException
     * @throws NoSuchMethodException
     */
    private static void transformFromCorbaType(Object serviceObject,
            SearchedType sType, Object corbaObject,
            RuntimeInformation runtimeInfo, boolean isParam, Field parentField,
            Object parentObject) throws SecurityException,
            NoSuchFieldException, IllegalArgumentException,
            IllegalAccessException, ClassNotFoundException,
            InvocationTargetException, InstantiationException,
            NoSuchMethodException {
        LOG.debug("Transform From Corba Type");
        if (serviceObject.getClass().isArray()) {
            LOG.debug("!!!!>>>>>> IsARRAY");
            Object[] corbaObjectArray = (Object[]) corbaObject;
            Object[] serviceObjectArray = (Object[]) serviceObject;
            LOG.debug("!!!!>>>>>> arrayLent: " + corbaObjectArray.length);
            LOG.debug("!!!!>>>>>> component type: " + corbaObject.getClass().getComponentType());
            for (int i = 0; i < corbaObjectArray.length; i++) {
                // Invertire Original con modified
                serviceObjectArray[i] = serviceObject.getClass().getComponentType().newInstance();
                // origObjectArray[i] =
                // origObject.getClass().getComponentType().newInstance();
                transformFromCorbaType(serviceObjectArray[i], sType,
                        corbaObjectArray[i], runtimeInfo, isParam, parentField,
                        parentObject);
                LOG.debug("!!!!>>>>>> component type object: " + corbaObjectArray[i]);
            }

        } else {
            if (sType instanceof UnionType) {
                // transform from unionType
                Object fieldValue = serviceObject;
                Field utWrapperField = null;

                Class srvObject = runtimeInfo.getServiceClassLoader().loadClass(
                        TypeUtils.getTypeNameWithoutBrackets(serviceObject.getClass()));
                utWrapperField = srvObject.getDeclaredField(UnionTypeUtils.UNION_WRAPPER_FIELD);
                utWrapperField.setAccessible(true);
                // fieldValue = new Object();
                // utWrapperField.set(modifiedObject,fieldValue);


                for (String fieldName : ((UnionType) sType).getTypeFieldNameList()) {
                    Class corbaObjectClass = runtimeInfo.getCorbaClassLoader().loadClass(
                            TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
                    Method crbObjectMeth = corbaObjectClass.getMethod(fieldName);

                    try {
                        Object unionFieldObject = crbObjectMeth.invoke(corbaObject);
                        boolean isFound = false;
                        if (TypeUtils.isPrimitive(unionFieldObject.getClass().getName()) || TypeUtils.isJavaType(unionFieldObject.getClass().getName()) || TypeUtils.isPrimitiveParamSign(unionFieldObject.getClass().getName())) {
                            utWrapperField.set(serviceObject,
                                    unionFieldObject);
                            isFound = true;
                        } else if (TypeUtils.isEnumType(unionFieldObject.getClass(), runtimeInfo.getAllEnumTypes().keySet())) {
                            utWrapperField.set(
                                    serviceObject,
                                    transformEnumType(
                                    unionFieldObject,
                                    runtimeInfo.getCorbaClassLoader(),
                                    runtimeInfo.getServiceClassLoader()));
                            isFound = true;
                        } else {
                            if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(
                                    unionFieldObject.getClass()))) {
                                fieldValue = new AnyTypeWrapper();
                            } else {
                                if (unionFieldObject.getClass().isArray()) {
                                    fieldValue = Array.newInstance(
                                            runtimeInfo.getServiceClassLoader().loadClass(
                                            TypeUtils.getTypeNameWithoutBrackets(unionFieldObject.getClass())),
                                            TypeUtils.getObjectDimensions(unionFieldObject));
                                } else {
                                    fieldValue = runtimeInfo.getServiceClassLoader().loadClass(
                                            unionFieldObject.getClass().getName()).newInstance();

                                }
                                utWrapperField.set(serviceObject,
                                        fieldValue);
                            }
                        }

                        if (!isParam) {
                            parentField.set(parentObject, serviceObject);
                        }
                        if (isFound) {
                            break;
                        }
                        LOG.debug("!!!!>>>>>> union field inited: " + fieldName + " value: " + unionFieldObject);
                        SearchedType searchedType = TypeUtils.isSearchedTypeAll(
                                TypeUtils.getTypeNameWithoutBrackets(unionFieldObject.getClass()), false,
                                runtimeInfo.getAllCorbaTypes());
                        if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(
                                unionFieldObject.getClass()))) {
                            searchedType = new AnyType();
                        }
                        if (searchedType != null) {

                            transformFromCorbaType(fieldValue, searchedType,
                                    unionFieldObject, runtimeInfo, false,
                                    parentField, parentObject);

                            if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(
                                    unionFieldObject.getClass()))) {

                                utWrapperField.set(serviceObject,
                                        ((AnyTypeWrapper) fieldValue).getValue());
                                if (!isParam) {
                                    parentField.set(parentObject,
                                            ((AnyTypeWrapper) fieldValue).getValue());
                                }
                            }
                        } else {
                            buildServiceObjectfromCorbaObject(fieldValue,
                                    unionFieldObject, runtimeInfo);
                        }
                        break;

                    } catch (InvocationTargetException ex) {
                        // ignore exception - union field is not initialized
                        // this will be thrown until finding the initialized
                        // field
                        LOG.debug("EXCEPTION: " + ex.getClass() + " message: " + ex.getMessage());
                    }
                }
            }
            //Fixed
//                        if (sType instanceof InterfaceType) {
//				// transform from interface
//                                // 
//                                // 
//                        }

            if (sType instanceof AnyType) {
                LOG.debug(">>>> Is Any:");
                if (corbaObject.getClass().isArray()) {
                    Object result = Array.newInstance(Object.class, TypeUtils.getObjectDimensions(corbaObject));
                    Object[] resultArray = (Object[]) result;
                    Object[] corbaObjectArray = (Object[]) corbaObject;
                    for (int i = 0; i < resultArray.length; i++) {
                        AnyTypeWrapper anyWr = new AnyTypeWrapper();
                        transformFromCorbaType(anyWr, sType, corbaObjectArray[i], runtimeInfo, isParam, parentField, parentObject);
                        resultArray[i] = anyWr.getValue();
                    }
                    ((AnyTypeWrapper) serviceObject).setValue(resultArray);
                } else {
                    Object result = transformFromAny((Any) corbaObject, null, null, runtimeInfo);
                    ((AnyTypeWrapper) serviceObject).setValue(result);
                }

            }
        }
    }

    /**
     * transformFromAny - Transform Corba objects to jax-ws objects (Any type)
     *
     * @param paramAny
     * @param runtimeInfo
     * @return
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws ClassNotFoundException
     * @throws InvocationTargetException
     * @throws InstantiationException
     * @throws NoSuchMethodException
     */
    private static Object transformFromAny(Any paramAny, TypeCode typeCode, InputStream anyInputStream,
            RuntimeInformation runtimeInfo) throws SecurityException,
            NoSuchFieldException, IllegalArgumentException,
            IllegalAccessException, ClassNotFoundException,
            InvocationTargetException, InstantiationException,
            NoSuchMethodException {
        Object localObject = null;
        
        
        // TYPEDEF in an ANY *******************
        // The typeCode is always null when extracting a typeDef from an Any:
        if (typeCode == null) {
        	try {
        		String typeId = paramAny.type().id();
        		TypeDef typeDef = TypeDefUtil.getTypeDefFromID(runtimeInfo.getTypeDefs(), typeId);
        			if (LOG.isDebugEnabled()) {
        				LOG.debug("ID found: "  + typeId);
        		}
        		LOG.debug("TypeDef found: "  + typeDef);
        		// Here also with the typedef!
        		if (typeDef != null) {
        			LOG.debug(">>>> typedef, value: " + paramAny);
        			TypeCode localTypeCode1 = null;
        			try {
        				localTypeCode1 = paramAny.type().content_type();
        			} catch (BadKind b) {
        				LOG.debug("Not a type with an ID...see CORBA specs. Cannot be a typedef...");
        			}
        			// Recursive call...NOW should NOT control typedefs...
        			localObject = transformFromAny(paramAny, localTypeCode1, anyInputStream, runtimeInfo);
        			// But we cannot return it, since it's a Typedef.
        			if (LOG.isDebugEnabled()) {
        				LOG.debug("Found TypeDef of class: " + localObject.getClass().getName() + "with ID:" + typeId);
        			}

        			Object obj = typeDef.getTypeDefObject(runtimeInfo.getServiceClassLoader(), localObject);
        			return obj;        		                                                                
        		}
        	} catch (BadKind b) {        		
        		LOG.debug("Not a type with an ID...see CORBA specs. Cannot be a typedef...");
        	}
        }
        // TYPEDEF end *********
                
        TCKind localTCKind = (typeCode == null) ? paramAny.type().kind() : typeCode.kind();
        if (localTCKind.equals(TCKind.tk_string)) {
            if (anyInputStream == null) {
            	LOG.debug("REading string from any");
                localObject = paramAny.extract_string();
            } else {
            	LOG.debug("REading string from inputstream");
                localObject = anyInputStream.read_string();
            }
            LOG.debug(">>>> type: string, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_long)) {
            if (anyInputStream == null) {
                localObject = new Integer(paramAny.extract_long());
            } else {
                localObject = new Integer(anyInputStream.read_long());
            }

            LOG.debug(">>>> type: long, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_double)) {
            if (anyInputStream == null) {
                localObject = new Double(paramAny.extract_double());
            } else {
                localObject = new Double(anyInputStream.read_long());
            }

            LOG.debug(">>>> type: double, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_boolean)) {
            if (anyInputStream == null) {
                localObject = new Boolean(paramAny.extract_boolean());
            } else {
                localObject = new Boolean(anyInputStream.read_boolean());
            }

            LOG.debug(">>>> type: boolean, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_float)) {
            if (anyInputStream == null) {
                localObject = new Float(paramAny.extract_float());
            } else {
                localObject = new Float(anyInputStream.read_float());
            }

            LOG.debug(">>>> type: float, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_char)) {
            if (anyInputStream == null) {
                localObject = new Character(paramAny.extract_char());
            } else {
                localObject = new Character(anyInputStream.read_char());
            }

            LOG.debug(">>>> type: char, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_fixed)) {
            if (anyInputStream == null) {
                localObject = paramAny.extract_fixed();
            } else {
                localObject = anyInputStream.read_fixed();
            }

            LOG.debug(">>>> type: fixed, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_longdouble)) // ??
        {
            if (anyInputStream == null) {
                localObject = new Double(paramAny.extract_double());
            } else {
                localObject = new Double(anyInputStream.read_double());
            }

            LOG.debug(">>>> type: longDouble, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_longlong)) {
            if (anyInputStream == null) {
                localObject = new Long(paramAny.extract_longlong());
            } else {
                localObject = new Long(anyInputStream.read_longlong());
            }

            LOG.debug(">>>> type: longlong, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_null)) {
            localObject = null;
            LOG.debug(">>>> type: null, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_octet)) {
            if (anyInputStream == null) {
                localObject = new Byte(paramAny.extract_octet());
            } else {
                localObject = new Byte(anyInputStream.read_octet());
            }

            LOG.debug(">>>> type: octet, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_short)) {
            if (anyInputStream == null) {
                localObject = new Short(paramAny.extract_short());
            } else {
                localObject = new Short(anyInputStream.read_short());
            }

            LOG.debug(">>>> type: ushort, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_ushort)) {
            if (anyInputStream == null) {
                localObject = new Short(paramAny.extract_ushort());
            } else {
                localObject = new Short(anyInputStream.read_ushort());
            }

            LOG.debug(">>>> type: ushort, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_ulong)) {
            if (anyInputStream == null) {
                localObject = new Long(paramAny.extract_ulong());
            } else {
                localObject = new Long(anyInputStream.read_ulong());
            }

            LOG.debug(">>>> type: ushort, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_ulonglong)) {
            if (anyInputStream == null) {
                localObject = new Long(paramAny.extract_ulonglong());
            } else {
                localObject = new Long(anyInputStream.read_ulonglong());
            }

            LOG.debug(">>>> type: ushort, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_wchar)) {
            if (anyInputStream == null) {
                localObject = new Character(paramAny.extract_wchar());
            } else {
                localObject = new Character(anyInputStream.read_wchar());
            }

            LOG.debug(">>>> type: wchar, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_wstring)) {
            if (anyInputStream == null) {
                localObject = new String(paramAny.extract_wstring());
            } else {
                localObject = new String(anyInputStream.read_wstring());
            }

            LOG.debug(">>>> type: wstring, value: " + localObject);
        } else if (localTCKind.equals(TCKind.tk_any)) {
            if (anyInputStream == null) {
                localObject = transformFromAny(paramAny.extract_any(), null, null, runtimeInfo);
            } else {
                localObject = transformFromAny(anyInputStream.read_any(), null, null, runtimeInfo);
            }

            LOG.debug(">>>> type: any, value: " + localObject);
        } else if ((localTCKind.equals(TCKind.tk_array))) {
            // localObject = transformFromAny(paramAny, orb, serviceDescriptor);
            LOG.debug(">>>> type: array, value: " + paramAny);
            TypeCode localTypeCode1 = null;
            try {


                if (typeCode == null) {
                    localTypeCode1 = paramAny.type();
                } else {
                    localTypeCode1 = typeCode;
                }
                List<Integer> idxs = new ArrayList<Integer>();
                
                while (localTypeCode1.kind().equals(TCKind.tk_array)) {
                    idxs.add(localTypeCode1.length());
                    LOG.debug(">>>> type: array length: " + localTypeCode1);
                    LOG.debug(">>>> type: array length: " + localTypeCode1.length());
                    localTypeCode1 = localTypeCode1.content_type();
                }
                
                LOG.debug(">>>> TYPE array(Content) " + localTypeCode1.kind().value());

                Integer[] arrayIndexes =  idxs.toArray(new Integer[]{});
                int[] arrayIndexesInt =  new int[arrayIndexes.length];
                for (int i = 0; i < arrayIndexes.length; i++) {
                	arrayIndexesInt[i] = arrayIndexes[i].intValue();
                }
                int totalArrayLentgh = 1;
                for (Integer leng : idxs) {
                	totalArrayLentgh = totalArrayLentgh * leng;
                }
                if (LOG.isDebugEnabled()) {
                	LOG.debug(">>>> type: array TOTAL length: " + totalArrayLentgh);
                	LOG.debug("Array dimensions: " + ArrayUtils.toString(arrayIndexesInt));
                	LOG.debug("Array first dimensions: " + arrayIndexes.length);
                }
                
                
                InputStream inpStream = paramAny.create_input_stream();               

                if (localTypeCode1.kind().equals(TCKind.tk_union) || localTypeCode1.kind().equals(TCKind.tk_struct) || localTypeCode1.kind().equals(TCKind.tk_value) || localTypeCode1.kind().equals(TCKind.tk_value_box) || localTypeCode1.kind().equals(TCKind.tk_enum) || localTypeCode1.kind().equals(TCKind.tk_except) || localTypeCode1.kind().equals(TCKind.tk_native) || localTypeCode1.kind().equals(TCKind.tk_abstract_interface)) {

                    localObject = transformFromAny(paramAny, localTypeCode1, inpStream, runtimeInfo);

                } else {
                	Object[] objects = new Object[totalArrayLentgh];
                	for (int i = 0;   i < totalArrayLentgh; i++) {                			
                		LOG.debug("--->TransForm Array -->" + i);
                		// The array Object
                		Object object = transformFromAny(paramAny, localTypeCode1, inpStream, runtimeInfo);
                		LOG.debug("Recovered form array: " + object);
                		objects[i] = object;                		                	
                	}
                	                	                	                       
                	// Creates the multidimensiona array from the linearized oject
                	Object myArray = Array.newInstance(Object.class, arrayIndexesInt);                	
                	delinearizeArray(objects, myArray, 0);
      	
                	localObject = myArray;
                }


            } catch (BadKind e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

//        } else if (localTCKind.equals(TCKind.tk_alias)) {
//        	// Here also with the typedef!
//        	
//            LOG.debug(">>>> type: alias, value: " + paramAny);
//            TypeCode localTypeCode1 = null;
//            try {            	
//                if (typeCode == null) {
//                    localTypeCode1 = paramAny.type().content_type();
//                } else {
//                    localTypeCode1 = typeCode.content_type();
//                }            	
//            	
//        	    // Now, Obj is a "wrapped Object". We have to convert it to the typeDef.        		        		
//        		// Recursive call...
//        		localObject = transformFromAny(paramAny, localTypeCode1, null, runtimeInfo);
//        		// But we cannot return it, since it's a Typedef.
//        		LOG.info("Found TypeDef of class: " + localObject.getClass().getName() + "with ID:" + paramAny.type().id());
//
//        		TypeDef typeDef = TypeDefUtil.getTypeDefFromID(runtimeInfo.getTypeDefs(), paramAny.type().id());        		        		
//        		Object obj = typeDef.getTypeDefObject(runtimeInfo.getServiceClassLoader(), localObject);
//        		return obj;        		                                                                
//
//            } catch (BadKind e) {
//                // TODO Auto-generated catch block
//                e.printStackTrace();
//            }

        } else if (localTCKind.equals(TCKind.tk_sequence)) {
            try {

                LOG.debug(">>>> type: sequence, value: " + paramAny);

                TCKind localTCKind2 = paramAny.type().content_type().content_type().kind();

                if (localTCKind2.equals(TCKind.tk_string)) {
                    localObject = StringSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - string, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_long)) {
                    localObject = LongSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - long, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_double)) {
                    localObject = DoubleSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - double, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_boolean)) {
                    localObject = BooleanSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - boolean, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_float)) {
                    localObject = FloatSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - float, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_char)) {
                    localObject = CharSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - char, value: " + localObject);

                } else if (localTCKind2.equals(TCKind.tk_longdouble)) // ??
                {
                    localObject = DoubleSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - longDouble, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_longlong)) {
                    localObject = LongLongSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - longlong, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_null)) {
                    localObject = null;
                    LOG.debug(">>>> type: null, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_octet)) {
                    localObject = OctetSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - octet, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_short)) {
                    localObject = ShortSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - ushort, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_ushort)) {
                    localObject = UShortSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - ushort, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_ulong)) {
                    localObject = ULongSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - ushort, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_ulonglong)) {
                    localObject = ULongLongSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - ushort, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_wchar)) {
                    localObject = WCharSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - wchar, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_wstring)) {
                    localObject = WStringSeqHelper.extract(paramAny);
                    LOG.debug(">>>> type: sequence - wstring, value: " + localObject);
                } else if (localTCKind2.equals(TCKind.tk_any)) {

                    Any[] anys = AnySeqHelper.extract(paramAny);

                    Object[] arrayOfObject = new Object[anys.length];

                    for (int j = 0; j < anys.length; ++j) {
                        arrayOfObject[j] = transformFromAny(
                                anys[j], null, null, runtimeInfo);
                    }

                    localObject = arrayOfObject;
                    LOG.debug(">>>> type: sequence - any, value: " + localObject);
                } else {
                    LOG.debug(">>>> type:  sequence - complex, value: " + localObject);
                    //3-10-2009 CRB-240 any containing a sequence.
                    //If is a redefined sequence get the Correct Helper and extract the class
                    String corbaObjClassStr = null;
                    //**************************************
                    //Extract Helper from Map
                    corbaObjClassStr = runtimeInfo.getIdTOClassNameMap().get(paramAny.type().id());
                    //Added to fix idljbug

                    if (corbaObjClassStr == null) {
                        String lastElem = paramAny.type().id().substring(paramAny.type().id().lastIndexOf("/"), paramAny.type().id().length());
                        String vers = lastElem.substring(lastElem.indexOf(":"));
                        corbaObjClassStr = runtimeInfo.getIdTOClassNameMap().get(paramAny.type().id().substring(0, paramAny.type().id().lastIndexOf("/")) + vers);
                    }
                    if (corbaObjClassStr != null) {

                        LOG.debug("---->" + corbaObjClassStr);

                        //Object Class

                        Object arrayOfObject = TypeUtils.extractCorbaTypeFromAny(paramAny, corbaObjClassStr, runtimeInfo.getCorbaClassLoader());
                        //Service Class

                        //  Calculate the class name without squares                        
                        // String clazzName = arrayOfObject.getClass().getCanonicalName().substring(0, arrayOfObject.getClass().getCanonicalName().length() - 2);
                        Class cl = arrayOfObject.getClass();                        
                        while (cl.isArray()) {
                        	cl = cl.getComponentType();
                        }
                        String clazzName = cl.getCanonicalName();

                        Class contentClazz = null;
                        
                        if (!TypeUtils.isPrimitive(clazzName)) {
                            contentClazz = runtimeInfo.getServiceClassLoader().loadClass(clazzName);
                            // Object result = Array.newInstance(contentClazz, TypeUtils.getObjectDimensions(arrayOfObject)[0]);
                            Object result = Array.newInstance(contentClazz, TypeUtils.getObjectDimensions(arrayOfObject));
                            //Transform from Corba to Service
                            buildServiceObjectfromCorbaObject(result, arrayOfObject, runtimeInfo);
                            localObject = result;

                        } else {

                            LOG.debug("Is Primitive");
                            contentClazz = TypeUtils.getJavaLangClassPrimitive(clazzName);
                            localObject = arrayOfObject;

                        }
                        if (LOG.isDebugEnabled()) {
                        	LOG.debug("Returning array: " + ArrayUtils.toString(localObject));
                        }

                    } else {

                        LOG.debug(">>>> type: sequence - unknown, value: " + paramAny.type().name() + " component: " +
                                paramAny.type().content_type().content_type().kind().value());

                    }

                }
            } catch (BadKind e) {
                // invalid sequence
                LOG.debug("Invalid Type: " + e.getMessage());
            }
        } else if (localTCKind.equals(TCKind.tk_objref)) {
            // TODO how to treat org.omg.CORBA.Object
            localObject = paramAny.extract_Object();
            LOG.debug(">>>> type: objref, value: " + paramAny);

        } else if (localTCKind.equals(TCKind.tk_union) || localTCKind.equals(TCKind.tk_struct) || localTCKind.equals(TCKind.tk_value) || localTCKind.equals(TCKind.tk_value_box) || localTCKind.equals(TCKind.tk_enum) || localTCKind.equals(TCKind.tk_except) || localTCKind.equals(TCKind.tk_native) || localTCKind.equals(TCKind.tk_abstract_interface)) {
            String corbaObjClassStr = null;
            String corbaObjClassNoImplStr = null;  
            try {
                LOG.debug(">>>> TYPECODE ID: " + paramAny.type().id());

                corbaObjClassStr = runtimeInfo.getIdTOClassNameMap().get(paramAny.type().id());
                if (corbaObjClassStr == null) {
                    String lastElem = paramAny.type().id().substring(paramAny.type().id().lastIndexOf("/"), paramAny.type().id().length());
                    String vers = lastElem.substring(lastElem.indexOf(":"));
                    corbaObjClassStr = runtimeInfo.getIdTOClassNameMap().get(paramAny.type().id().substring(0, paramAny.type().id().lastIndexOf("/")) + vers);
                }
                LOG.debug(">>>> ASSOCIATED CLASS : " + corbaObjClassStr);
            } catch (BadKind e) {
                // invalid sequence
                LOG.debug("Invalid Type: " + e.getMessage());
            }
            
            Object corbaObj = null;
            if (anyInputStream == null) {
                corbaObj = TypeUtils.extractCorbaTypeFromAny(paramAny, corbaObjClassStr, runtimeInfo.getCorbaClassLoader());
            } else {
                corbaObj = TypeUtils.readCorbaTypeFromAnyInputStream(anyInputStream, corbaObjClassStr, runtimeInfo.getCorbaClassLoader());
            }
            
            String corbaObjClassName = corbaObj.getClass().getName();
            if ((localTCKind.equals(TCKind.tk_value) || localTCKind.equals(TCKind.tk_value_box)) && corbaObjClassName.endsWith("Impl")) {
                // remove "Impl" from value types            	
            	corbaObjClassNoImplStr = corbaObjClassName.substring(0, corbaObjClassName.length() - 4);
            } else {
            	corbaObjClassNoImplStr = corbaObjClassName;
            }

            if (LOG.isDebugEnabled()) {
            	LOG.debug("corbaObjClassNoImplStr: " + corbaObjClassNoImplStr);
            	LOG.debug(">>>> type: union or struct, value: " + corbaObj + " \n class: " + corbaObj.getClass());
            }
            
            SearchedType st = TypeUtils.isSearchedTypeAll(TypeUtils.getTypeNameWithoutBrackets(corbaObj.getClass()), false,
                    runtimeInfo.getAllCorbaTypes());
            
            LOG.debug("Searched type: " + st);
            if (st != null) {
                AnyTypeWrapper anyWr = new AnyTypeWrapper();
                Field anyWrField = anyWr.getClass().getDeclaredField("value");
                Object srvObj = new Object();
                if (st instanceof UnionType) {
                    Class utWrapperClass = runtimeInfo.getServiceClassLoader().loadClass(st.getTypeName() + "Wrapper");
                    if (corbaObj.getClass().isArray()) {
                        srvObj = Array.newInstance(utWrapperClass,
                                TypeUtils.getObjectDimensions(corbaObj));
                    } else {
                        srvObj = utWrapperClass.newInstance();
                    }

                }

                transformFromCorbaType(srvObj, st, corbaObj, runtimeInfo,
                        false, anyWrField, anyWr);

                localObject = anyWr.getValue();
            } else {
                for (Class type : runtimeInfo.getAllIDLTypes()) {
                	// Uses the corbaObjClassStr, since if VT, the impl is not used.
                	String typeNameWithoutBrackets = TypeUtils.getTypeNameWithoutBrackets(corbaObjClassNoImplStr);
                	LOG.debug("TypeNameWithoutBrackets : " + typeNameWithoutBrackets + ", comparing with " + type.getName());
                    if (typeNameWithoutBrackets.equals(type.getName())) {
                    	LOG.debug("Found :" + type.getName());
                        Class serviceObjClass = runtimeInfo.getServiceClassLoader().loadClass(
                                TypeUtils.getTypeNameWithoutBrackets(type));
                        Object serviceObj = null;
                        LOG.debug("corbaObj.getClass(): " + corbaObj.getClass().getName());
                        

                        //Added to fix Any of Enum
                        if (TypeUtils.isEnumType(corbaObj.getClass(), runtimeInfo.getAllEnumTypes().keySet())) {

                            serviceObj = transformEnumType(corbaObj, runtimeInfo.getCorbaClassLoader(),
                                    runtimeInfo.getServiceClassLoader());
                            localObject = serviceObj;

                            break;

                        } else if (corbaObj.getClass().isArray()) {
                            serviceObj = Array.newInstance(serviceObjClass,
                                    TypeUtils.getObjectDimensions(corbaObj));
                        } else {
                            serviceObj = serviceObjClass.newInstance();
                        }
                        buildServiceObjectfromCorbaObject(serviceObj, corbaObj,
                                runtimeInfo);
                        localObject = serviceObj;
                        break;
                    }
                }
            }
        }

        return localObject;
    }

    /**
     * buildCorbaObjectfromServiceObject - Transform original corba objects for servant
     * invocation
     *
     * @param corbaObject
     * @param serviceObject
     * @param runtimeInfo
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws ClassNotFoundException
     * @throws NoSuchMethodException
     * @throws InstantiationException
     */
    private static void buildCorbaObjectfromServiceObject(Object corbaObject,
            Object serviceObject, RuntimeInformation runtimeInfo)
            throws SecurityException, NoSuchFieldException,
            IllegalArgumentException, IllegalAccessException,
            InvocationTargetException, ClassNotFoundException,
            NoSuchMethodException, InstantiationException {

        LOG.debug("Oirginal Object Class Loader ===>" + corbaObject.getClass().getClassLoader());
        LOG.debug("Modified Object Class Loader ===>" + serviceObject.getClass().getClassLoader());
        // field is primitive or java type

        LOG.debug("!!!!>>>>>> origObject: " + corbaObject + " class: " + corbaObject.getClass() + " classloader: " + corbaObject.getClass().getClassLoader());
        LOG.debug("!!!!>>>>>> modifiedObject: " + serviceObject + " class: " + serviceObject.getClass() + " classloader: " + serviceObject.getClass().getClassLoader());
        if (corbaObject.getClass().isArray()) {
            LOG.debug("!!!!>>>>>> IsARRAY");
            Object[] corbaObjectArray = (Object[]) corbaObject;
            Object[] serviceObjectArray = (Object[]) serviceObject;
            LOG.debug("!!!!>>>>>> arrayLent: " + corbaObjectArray.length);
            LOG.debug("!!!!>>>>>> component type: " + corbaObject.getClass().getComponentType());
            for (int i = 0; i < corbaObjectArray.length; i++) {
                if (serviceObjectArray[i].getClass().isArray()) {
                    LOG.debug("!!!!>>>>>> embeded arrayLent: " + ((Object[]) serviceObjectArray[i]).length);
                    for (int j = 0; j < ((Object[]) serviceObjectArray[i]).length; j++) {
                        LOG.debug("!!!!>>>>>> embeded array: value " + j + " - " + ((Object[]) serviceObjectArray[i])[j]);
                    }
                    corbaObjectArray[i] = Array.newInstance(
                            runtimeInfo.getCorbaClassLoader().loadClass(
                            TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass())),
                            TypeUtils.getObjectDimensions(serviceObjectArray[i]));
                } else {
                    corbaObjectArray[i] = corbaObject.getClass().getComponentType().newInstance();
                }

                if (TypeUtils.isPrimitive(corbaObjectArray[i].getClass().getName()) || TypeUtils.isJavaType(corbaObjectArray[i].getClass().getName()) || TypeUtils.isPrimitiveParamSign(corbaObjectArray[i].getClass().getName())) {
                    corbaObjectArray[i] = serviceObjectArray[i];
                }/* else if (TypeUtils.isEnumType(corbaObjectArray[i].getClass(),
                runtimeInfo.getAllEnumTypes().keySet())) {
                corbaObjectArray[i] = transformEnumType(
                serviceObjectArray[i], runtimeInfo
                .getServiceClassLoader(), runtimeInfo
                .getCorbaClassLoader());

                }*/ else {
                    buildCorbaObjectfromServiceObject(corbaObjectArray[i],
                            serviceObjectArray[i], runtimeInfo);
                }
                LOG.debug("!!!!>>>>>> component type object: " + corbaObjectArray[i]);
            }

        } else {

            Class corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                    TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
            Class serviceClass = runtimeInfo.getServiceClassLoader().loadClass(
                    TypeUtils.getTypeNameWithoutBrackets(serviceObject.getClass()));

            List<Field> allCrbFields = new ArrayList<Field>();
            getAllFields(corbaClass, allCrbFields);
            Field[] crbFields = allCrbFields.toArray(new Field[allCrbFields.size()]);// origClass.getDeclaredFields();
            for (int i = 0; i < crbFields.length; i++) {
                crbFields[i].setAccessible(true);
                Field srvField = getField(serviceClass, crbFields[i].getName());// modClass.getDeclaredField(origFields[i].getName());
                srvField.setAccessible(true);
                if (srvField.get(serviceObject) == null) {
                    LOG.warn("Warning: Field " + srvField.getName() + " from service object " + serviceObject + " is null");
                    continue;
                }
                // field is primitive or java type
                LOG.debug("!!!!>>>>>> origFieldTypeName: " + crbFields[i].getType().getName());
                LOG.debug("!!!!>>>>>> modifField: " + srvField.getType().getName());
                if (crbFields[i].isAccessible()) {
	                if (TypeUtils.isPrimitive(crbFields[i].getType().getName()) || TypeUtils.isJavaType(crbFields[i].getType().getName()) || TypeUtils.isPrimitiveParamSign(crbFields[i].getType().getName())) {
	                    LOG.debug("PRIMITIVE OR JAVATYPE");
	                    crbFields[i].set(corbaObject, srvField.get(serviceObject));	                    
	                } else if (TypeUtils.isEnumType(crbFields[i].getType(),
	                        runtimeInfo.getAllEnumTypes().keySet())) {
	                    crbFields[i].set(corbaObject, transformEnumType(srvField.get(serviceObject), runtimeInfo.getServiceClassLoader(), runtimeInfo.getCorbaClassLoader()));
	
	                } // complex type
	                else {
	
	                    String crbFieldTypeName = crbFields[i].getType().getName();
	                    SearchedType ut = TypeUtils.isSearchedTypeAll(
	                            crbFieldTypeName, crbFieldTypeName.startsWith("["), runtimeInfo.getAllCorbaTypes());
	                    if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(
	                            crbFields[i].getType()))) {
	                        ut = new AnyType();
	                    }
	                    // field is corba type
	
	                    if (ut != null) {
	                        LOG.debug("TRANSFORM==>2 *****************************************");
	                        LOG.debug("===>" + crbFields[i].toString());
	                        LOG.debug("===>" + corbaObject.toString());
	                        LOG.debug("===>" + crbFields[i].getGenericType());
	                        LOG.debug("===>" + crbFields[i].getName());
	
	                        LOG.debug("===>" + ut.getClass());
	                        LOG.debug("===>" + serviceObject.toString());
	                        LOG.debug("*******************************************************");
	
	                        if (ut instanceof InterfaceType) {
	                            String ior = null;
	                            if (srvField.get(serviceObject) instanceof W3CEndpointReference) {
	                                W3CEndpointReference epr = (W3CEndpointReference) srvField.get(serviceObject);
	
	                                LOG.debug("Document Fragment EPR ===> " + epr.toString());
	                                ior = HelperEPRUtils.readAddressFromEPR(epr.toString());
	                                LOG.debug("IOR ===> " + ior);
	
	                            }
	                            // ORB orb =
	                            // serviceDescriptor.getEndpoint().getOrb();
	                            LOG.debug("IOR ===> " + ior);
	
	                            // Load Helper and Do the narrow
	                            // Class helperClass =
	                            // originalClassLoader.loadClass(origFields[i].getType().getName()
	                            // + "Helper");
	                            Class helperClass = loadHelperbyName(crbFields[i].getType().getName(), runtimeInfo.getCorbaClassLoader());
	                            crbFields[i].set(corbaObject, narrowObjbyIOR(
	                                    helperClass, ior, runtimeInfo.getOrb()));
	
	                        } else {
	
	                            Object fieldValue = null;
	                            if (AnyType.isAnyType(TypeUtils.getTypeNameWithoutBrackets(
	                                    crbFields[i].getType()))) {
	                                if (crbFields[i].getType().isArray()) {
	                                    fieldValue = new Any[((Object[]) srvField.get(serviceObject)).length];
	//									                     Array
	//											.newInstance(
	//													Any.class,
	//													TypeUtils
	//															.getObjectDimensions(modifiedObject));
	                                } else {
	                                    fieldValue = runtimeInfo.getOrb().create_any();
	
	                                }
	                            } else {
	                                if (crbFields[i].getType().isArray()) {
	                                    fieldValue = Array.newInstance(
	                                            runtimeInfo.getCorbaClassLoader().loadClass(
	                                            TypeUtils.getTypeNameWithoutBrackets(crbFields[i].getType())),
	                                            TypeUtils.getObjectDimensions(serviceObject));
	                                } else {
	                                    fieldValue = runtimeInfo.getCorbaClassLoader().loadClass(
	                                            crbFields[i].getType().getName()).newInstance();
	
	                                }
	                            }
	                            crbFields[i].set(corbaObject, fieldValue);
	                            transformToCorbaType(fieldValue, ut, srvField.get(serviceObject), runtimeInfo, false);
	                        }
	                    } // complex type but not corba type
	                    else {
	                        // Init TODO refactoring
	                        LOG.debug(" TYPE==>" + crbFields[i].getType());
	                        LOG.debug(" TYPE NAME==>" + crbFields[i].getType().getName());
	                        LOG.debug(" MODIFIED OBJECT==>" + srvField.getType().getName());
	
	                        // Create a new Instance for the Field
	                        if (crbFields[i].getType().getName().startsWith("[")) {
	                            Object[] obj = (Object[]) srvField.get(serviceObject);
	                            LOG.debug("Modifield Fild Array Lenght" + obj.length);
	                            LOG.debug(crbFields[i].getType().getComponentType().toString());
	                            crbFields[i].set(corbaObject, Array.newInstance(
	                                    crbFields[i].getType().getComponentType(),
	                                    obj.length));
	                        } else {
	                            crbFields[i].set(corbaObject, crbFields[i].getType().newInstance());
	                        }
	                        if (TypeUtils.isPrimitive(crbFields[i].get(corbaObject).getClass().getName()) || TypeUtils.isJavaType(crbFields[i].get(
	                                corbaObject).getClass().getName()) || TypeUtils.isPrimitiveParamSign(crbFields[i].get(corbaObject).getClass().getName())) {
	                            crbFields[i].set(corbaObject, srvField.get(serviceObject));
	                        } else if (TypeUtils.isEnumType(crbFields[i].get(
	                                corbaObject).getClass(), runtimeInfo.getAllEnumTypes().keySet())) {
	                            crbFields[i].set(corbaObject, transformEnumType(
	                                    srvField.get(serviceObject), runtimeInfo.getServiceClassLoader(),
	                                    runtimeInfo.getCorbaClassLoader()));
	
	                        } else {
	                            buildCorbaObjectfromServiceObject(crbFields[i].get(corbaObject), srvField.get(serviceObject), runtimeInfo);
	                        }
	                    }
	                }
                }
            }
        }

    }

    /**
     * transformToCorbaType treat special corba types case (as parameter/return
     * type and as field)
     *
     * @param corbaObject
     * @param ut
     * @param serviceObject
     * @param runtimeInfo
     * @param isParam
     * @throws ClassNotFoundException
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     * @throws InstantiationException
     */
    private static void transformToCorbaType(Object corbaObject,
            SearchedType ut, Object serviceObject,
            RuntimeInformation runtimeInfo, boolean isParam)
            throws ClassNotFoundException, SecurityException,
            NoSuchFieldException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException,
            NoSuchMethodException, InstantiationException {

        if (corbaObject.getClass().isArray()) {
            LOG.debug("!!!!>>>>>> IsARRAY");
            Object[] corbaObjectArray = (Object[]) corbaObject;
            Object[] serviceObjectArray = (Object[]) serviceObject;
            LOG.debug("!!!!>>>>>> arrayLent: " + corbaObjectArray.length);
            LOG.debug("!!!!>>>>>> modif arrayLent: " + serviceObjectArray.length);
            LOG.debug("!!!!>>>>>> component type: " + corbaObject.getClass().getComponentType());
            LOG.debug("!!!!>>>>>> modif component type: " + serviceObjectArray.getClass().getComponentType());
            for (int i = 0; i < corbaObjectArray.length; i++) {
                if (AnyType.isAnyType(corbaObject.getClass().getComponentType().getName())) {
                    corbaObjectArray[i] = runtimeInfo.getOrb().create_any();
                } else {
                    corbaObjectArray[i] = corbaObject.getClass().getComponentType().newInstance();
                }
                transformToCorbaType(corbaObjectArray[i], ut,
                        serviceObjectArray[i], runtimeInfo, isParam);
                LOG.debug("!!!!>>>>>> component type object: " + corbaObjectArray[i]);
            }

        } else {

            if (ut instanceof UnionType) {
                // transform to unionType
                LOG.debug("!!!!!>>> Instance of uniontype");
                Object fieldValue = serviceObject;

                String utWrapper = ut.getTypeName() + "Wrapper";
                Class utWrapperClass = runtimeInfo.getServiceClassLoader().loadClass(utWrapper);

                Field utWrapperField = utWrapperClass.getDeclaredField(UnionTypeUtils.UNION_WRAPPER_FIELD);
                LOG.debug("utWrapperClassString: " + utWrapper + " utWrapperClass: " + utWrapperClass + " utWrapperField: " + utWrapperField);
                utWrapperField.setAccessible(true);
                fieldValue = utWrapperField.get(serviceObject);
                LOG.debug("!!!!!>>> Field type" + fieldValue.getClass() + " value: " + fieldValue);

                LOG.debug("!!!!!>>> UnionFields: ");
                boolean fieldFound = false;
                String fieldAny = null;
                for (String fieldName : ((UnionType) ut).getTypeFieldNameList()) {
                    Class fieldType = ((UnionType) ut).getFieldType(fieldName);

                    // MARCO: it can be null in an Union with only one choice...
                    if (fieldType != null) {

                        Class typeToCompare = fieldType;
                        LOG.debug("!!!!!>>> Field type of union: " + fieldType);

                        Class primitiveJavaLangType = TypeUtils.getJavaLangClassPrimitive(fieldType.getName());
                        if (primitiveJavaLangType != null) {
                            typeToCompare = primitiveJavaLangType;
                        }
                        LOG.debug("!!!!!>>> Field type to compare: " + typeToCompare);
                        if (fieldValue.getClass().equals(typeToCompare)) {
                            LOG.debug("!!!!!>>> Field name: " + fieldName);
                            Class corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                                    TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
                            LOG.debug("!!!!!>>> CORBA class: " + corbaClass);
                            Method methodForfieldToFill = corbaClass.getMethod(
                                    fieldName, fieldType);
                            LOG.debug("!!!!!>>> methodForfieldToFill class: " + methodForfieldToFill);
                            LOG.debug("!!!!!>>> fieldValue: " + fieldValue);

                            methodForfieldToFill.invoke(corbaObject, fieldValue);
                            fieldFound = true;
                            break;

                        } else {
                            boolean isUnion = false;
                            String fieldTypeStr = TypeUtils.getTypeNameWithoutBrackets(fieldType);
                            String fieldValueTypeStr = TypeUtils.getTypeNameWithoutBrackets(fieldValue.getClass());
                            LOG.debug("!!!!!>>> Field type str: " + fieldTypeStr);
                            LOG.debug("!!!!!>>> Field Value type str: " + fieldValueTypeStr);

                            SearchedType st = TypeUtils.isSearchedTypeAll(
                                    fieldTypeStr, false, runtimeInfo.getAllCorbaTypes());
                            // search for union
                            if (st == null && fieldTypeStr.endsWith("Wrapper")) {
                                String tempFieldTypeStr = fieldTypeStr.substring(0, fieldTypeStr.length() - 7);
                                st = TypeUtils.isSearchedTypeAll(
                                        tempFieldTypeStr, false, runtimeInfo.getAllCorbaTypes());
                                if (st != null) {
                                    fieldTypeStr = tempFieldTypeStr;
                                    isUnion = true;
                                }
                            }
                            if (AnyType.isAnyType(fieldTypeStr)) {
                                st = new AnyType();
                                fieldAny = fieldName;
                            }
                            if (st != null) {
                                if (fieldValueTypeStr.equals("java.lang.Object") || isUnion) {
                                    LOG.debug("!!!!!>>> Is embeded anytype: " + st);
                                    Class corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                                            TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
                                    Class crbFieldClass = null;
                                    if (AnyType.isAnyType(fieldTypeStr)) {
                                        crbFieldClass = Any.class;
                                    } else {
                                        crbFieldClass = runtimeInfo.getCorbaClassLoader().loadClass(
                                                fieldTypeStr);
                                    }

                                    Object fieldSTValue = null;
                                    if (fieldValue.getClass().isArray()) {
                                        if (AnyType.isAnyType(fieldTypeStr)) {
                                            fieldSTValue = new Any[((Object[]) fieldValue).length];
                                        } else {
                                            int dimentions[] = TypeUtils.getObjectDimensions(fieldValue);
                                            fieldSTValue = Array.newInstance(
                                                    crbFieldClass, dimentions);
                                            LOG.debug("!!!!!>>> Is array: " + dimentions);
                                        }
                                    } else {
                                        if (AnyType.isAnyType(fieldTypeStr)) {
                                            fieldSTValue = runtimeInfo.getOrb().create_any();
                                        } else {
                                            fieldSTValue = fieldType.newInstance();
                                        }
                                    }
                                    transformToCorbaType(fieldSTValue, st, fieldValue,
                                            runtimeInfo, false);
                                    LOG.debug("!!!!!>>> Transformed: " + fieldSTValue);
                                    Method methodForfieldToFill = corbaClass.getMethod(
                                            fieldName, fieldSTValue.getClass());
                                    methodForfieldToFill.invoke(corbaObject,
                                            fieldSTValue);
                                    LOG.debug("!!!!!>>> Transformed full: " + corbaObject);
                                    fieldFound = true;
                                }
                            }
                        }
                    }
                }
                // if no field match and union has any field
                if (!fieldFound && fieldAny != null) {
                    LOG.debug("!!!!!>>>ANY in Union: " + fieldAny + " value: " + fieldValue);
                    Class fieldType = ((UnionType) ut).getFieldType(fieldAny);
                    Class corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(
                            TypeUtils.getTypeNameWithoutBrackets(corbaObject.getClass()));
                    Method methodForfieldToFill = corbaClass.getMethod(
                            fieldAny, fieldType);

                    methodForfieldToFill.invoke(corbaObject, transformToAny(fieldValue, null, runtimeInfo));
                }
            }

            if (ut instanceof AnyType) {

                LOG.debug("!!!!!>>> Is ANY type: Value " + serviceObject);

                transformToAny(serviceObject, ((org.omg.CORBA.Any) corbaObject),
                        runtimeInfo);
            }
        }

    }

    /**
     * * transformToAny - Transform jax-ws objects to Corba objects (Any type)
     *
     *
     * @param serviceObject
     * @param any
     * @param runtimeInfo
     * @return
     * @throws ClassNotFoundException
     * @throws SecurityException
     * @throws IllegalArgumentException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws NoSuchMethodException
     * @throws InstantiationException
     */
    public static Any transformToAny(Object serviceObject, Any any,
            RuntimeInformation runtimeInfo) throws ClassNotFoundException,
            SecurityException, IllegalArgumentException, NoSuchFieldException,
            IllegalAccessException, InvocationTargetException,
            NoSuchMethodException, InstantiationException {
        Any paramAny = runtimeInfo.getOrb().create_any();
        if (any != null) {
            paramAny = any;
        }
        if (serviceObject instanceof String) {
            paramAny.insert_string((String) serviceObject);
        } else if (serviceObject instanceof Long) {
            paramAny.insert_long(((Long) serviceObject).intValue());
        } else if (serviceObject instanceof Integer) {
            paramAny.insert_long(((Integer) serviceObject).intValue());
        } else if (serviceObject instanceof Double) {
            paramAny.insert_double(((Double) serviceObject).doubleValue());
        } else if (serviceObject instanceof Float) {
            paramAny.insert_float(((Float) serviceObject).floatValue());
        } else if (serviceObject instanceof Boolean) {
            paramAny.insert_boolean(((Boolean) serviceObject).booleanValue());
        } else if (serviceObject instanceof Character) {
            paramAny.insert_char(((Character) serviceObject).charValue());
        } else if (serviceObject instanceof Byte) {
            paramAny.insert_octet(((Byte) serviceObject).byteValue());
            LOG.debug(">>>> type: octet, value: " + serviceObject);
        } else if (serviceObject instanceof Short) {
            paramAny.insert_short(((Short) serviceObject).shortValue());
            LOG.debug(">>>> type: ushort, value: " + serviceObject);
        } else if (serviceObject instanceof BigInteger) {
            if (((BigInteger) serviceObject).toString().startsWith("-")) {
                paramAny.insert_string(((BigInteger) serviceObject).toString());
            } else {
                paramAny.insert_fixed(new BigDecimal((BigInteger) serviceObject));
            }

            LOG.debug(">>>> type: BigInteger -> fixed, value: " + serviceObject);
        } else if (serviceObject instanceof BigDecimal) {
            if (((BigDecimal) serviceObject).toString().startsWith("-")) {
                paramAny.insert_string(((BigDecimal) serviceObject).toString());
            } else {
                paramAny.insert_fixed((BigDecimal) serviceObject);
            }

            LOG.debug(">>>> type: BigInteger -> fixed, value: " + serviceObject);

        } else {
            LOG.debug(">>>> type: Other type(complex?), value: " + serviceObject);
            LOG.debug(">>>> Service Object Class ----> " + serviceObject.getClass());
            String paramObjectTypeStr = TypeUtils.getTypeNameWithoutBrackets(serviceObject.getClass());
            int dimentions[] = null;
            boolean isArray = serviceObject.getClass().isArray();
            if (isArray) {
                dimentions = TypeUtils.getObjectDimensions(serviceObject);
            }
            Class corbaClass = null;
            Object serviceObj = null;
            boolean classFound = false;

            for (Class type : runtimeInfo.getAllIDLTypes()) {
                LOG.debug("****Search Class for Any assignement************************");
                LOG.debug("Check for type paramObjectTypeStr --->" + paramObjectTypeStr);
                LOG.debug("Check for type type.getName--->" + type.getName());
                if (paramObjectTypeStr.equals(type.getName())) {

                    corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(paramObjectTypeStr);

                    if (TypeUtils.isEnumType(corbaClass, runtimeInfo.getAllEnumTypes().keySet()) && !isArray) {
                        LOG.debug("is Enum");
                        serviceObj = transformEnumType(serviceObject, runtimeInfo.getServiceClassLoader(), runtimeInfo.getCorbaClassLoader());
                    } else {
                        if (isArray) {
                            serviceObj = Array.newInstance(corbaClass, dimentions);

                        } else {
                            serviceObj = corbaClass.newInstance();
                        }

                        buildCorbaObjectfromServiceObject(serviceObj, serviceObject,
                                runtimeInfo);
                    }
                    LOG.debug(">>>> type: Complex, value: " + serviceObj);
                    classFound = true;
                    break;
                }
            }
            if (!classFound) {
                SearchedType st = TypeUtils.isSearchedTypeAll(
                        paramObjectTypeStr, false, runtimeInfo.getAllCorbaTypes());
                // check if is wrapper for union ( temporary: TODO change when completing task CRB-206 replace all union types with wrappers)
                if (st == null) {
                    if (paramObjectTypeStr.endsWith("Wrapper")) {
                        // remove "Wrapper" to look for union
                        paramObjectTypeStr = paramObjectTypeStr.substring(0, paramObjectTypeStr.length() - 7);
                        st = TypeUtils.isSearchedTypeAll(
                                paramObjectTypeStr, false, runtimeInfo.getAllCorbaTypes());
                    }
                }
                if (st != null) {
                    corbaClass = runtimeInfo.getCorbaClassLoader().loadClass(paramObjectTypeStr);
                    if (isArray) {
                        serviceObj = Array.newInstance(corbaClass, dimentions);
                    } else {
                        serviceObj = corbaClass.newInstance();
                    }

                    transformToCorbaType(serviceObj, st, serviceObject,
                            runtimeInfo, true);
                    LOG.debug(">>>> type: CorbaType(union, interface), value: " + serviceObj);
                    classFound = true;
                }
            }
            if (!classFound) {
                //OOOPS!!
                LOG.debug(">>>>!!!! type: Unknown, value: " + serviceObject);
            } else {
                TypeUtils.encloseCorbaTypeInAny(paramAny, serviceObj, corbaClass, runtimeInfo, runtimeInfo.getTypeDefs());
                LOG.debug(">>>> ANY : " + paramAny);
                LOG.debug(">>>> ANY kind: " + paramAny.type().kind());

            }

        }
        return paramAny;
    }

    /**
     * This method receive a Corba Object and trasform it to an
     * W3CEndpointReference
     *
     * @param Object
     *            the corba Object interface
     * @param jbi4corbaEndPoint
     * @return W3CEndpointReference
     * Class objectType
     */
    private static W3CEndpointReference objectToEPR(Object obj,
            Jbi4CorbaEndpoint jbi4corbaEndPoint, Class objectType) throws ClassNotFoundException {

        Jbi4CorbaSUManager su = jbi4corbaEndPoint.getSuManager();
        ProviderServiceDescriptor serviceDesc = null;
        Set<String> keys = new HashSet<String>();
        Set<String> classes = new HashSet<String>();
        String className = null;
        ProviderEndpoint pes = null;
        //-------------- Added To Manage Object ---------------------------
        //-----------------------------------------------------------------

        if (jbi4corbaEndPoint instanceof ProviderEndpoint) {
            pes = (ProviderEndpoint) jbi4corbaEndPoint;
            keys = pes.getServiceDescriptor().getIdToClassNameMap().keySet();
        }

        //If the interface is Corba Object find correct type with is a 
        if (objectType.equals(org.omg.CORBA.Object.class)) {
            //per ogni tipo della lista di interfacce conosciute controllo che obj is a se trovo il tipo break
            if (obj instanceof org.omg.CORBA.Object) {
                for (String s : keys) {
                    LOG.debug("Check class with name" + s);
                    if (((org.omg.CORBA.Object) obj)._is_a(s)) {
                        className = pes.getServiceDescriptor().getIdToClassNameMap().get(s);
                        //Remove Helper from String className
                        className = className.substring(0, className.length() - 6);

                        if (LOG.isDebugEnabled()) {
                            LOG.debug("----------------------------------------------------------------");
                            LOG.debug("Object Is A -->" + className);
                            LOG.debug("----------------------------------------------------------------");
                        }

                        classes.add(className);
                    }
                }
                //If there are more then one class search
                //Not Used.... To verify
//                        if(classes.size()>1){
//
//                            if(LOG.isDebugEnabled()){
//                                    LOG.debug("----------------------------------------------------------------");
//                                    LOG.debug("                        Check SuperClass                        ");
//                                    LOG.debug("----------------------------------------------------------------");
//                            }
//                            //Check the intersection of classes with superclasses
//                            for( String clazz:classes){
//                               Set<String> intersection=new HashSet<String>(classes);
//                               Class loadedClass = pes.getServiceDescriptor().getOriginalClassLoader().loadClass(clazz+"Operations");
//                               Set<String> superClasses= new HashSet<String>();
//                               getHierachy(loadedClass, superClasses);
//                               intersection.retainAll(superClasses);
//                               if(intersection.size()==0){
//                                   className= clazz;
//                                   break;
//                               }
//                            }
//
//                        }

                className = "interface " + className;
            }
        } else {
            className = obj.getClass().getInterfaces()[0].toString();
        }

        if (className == null) {
            throw new Jbi4CorbaRuntimeException("CRB--- The Interface don't exist in the SU");
        }
        //-------------------------------------------------------------

        // foreach deployed endpoint
        // TODO ---NullPointer Ecceptions
        for (Jbi4CorbaEndpoint endp : su.getDeployedEndpoints()) {

            if (endp instanceof ProviderEndpoint) {

                ProviderEndpoint pe = (ProviderEndpoint) endp;
                // We have to find the service that rappresent the returned
                // interface
                // if the obj is equals to the CorbaObjectInterface we can crate
                // an EPR from this
                // Service Descriptor
                String op = "Operations";
                String intName = pe.getServiceDescriptor().getServiceInterface().toString().substring(
                        0,
                        pe.getServiceDescriptor().getServiceInterface().toString().length() - op.length());
                if (LOG.isDebugEnabled()) {
                    LOG.debug("----------------------------------------------------------------");
                    LOG.debug("Result Interface -->" + className);
                    LOG.debug("Current Endpoint Interface -->" + intName);
                    LOG.debug("----------------------------------------------------------------");
                }

                if (className.equals(intName)) {
                    serviceDesc = pe.getServiceDescriptor();
                    /*The Reference is made by the endpoint that provide the interface*/
                    /*The destination endpoint */
                    jbi4corbaEndPoint = endp;
                    break;
                }
            }
        }

        ORB orb = serviceDesc.getEndpoint().getOrb();
        // Retrieve the IOR starting form Corba Object
        String ior = orb.object_to_string((org.omg.CORBA.Object) obj);

        return createEPR(ior, jbi4corbaEndPoint);

    }

    private static void getHierachy(Class clazz, Set<String> classHierarchy) throws ClassNotFoundException {

        if (clazz.getSuperclass() != null) {
            classHierarchy.add(clazz.getName());
            getHierachy(clazz.getSuperclass(), classHierarchy);
        }
        return;
    }

    /**
     * This Method generate a W3CEndpointReference from a IOR
     *
     * @param String
     *            ior
     * @param psd -
     *            ProviderServiceDescriptor
     * @return W3CEndpointRefernece
     */
    private static W3CEndpointReference createEPR(String ior,
            Jbi4CorbaEndpoint psd) {

        W3CEndpointReferenceBuilder builder = new W3CEndpointReferenceBuilder();
        // We have to generate a new Temporary endpoint deploy start it and
        // generate an EPR
        // For dynamic partnrelink associations
        QName serviceName = psd.getServiceName();
        QName endPointName = new QName(psd.getEndpointName());

        builder.address("jbi4corba:" + ior);
        builder.serviceName(serviceName);
        builder.endpointName(endPointName);
        LOG.debug("GENERATED EPR WITH >>>> address --> jbi4corba:" + ior + "\n serviceName -->" + serviceName.toString() + "\n endpointName -->" + endPointName.toString());
        W3CEndpointReference epr = builder.build();
        
        return epr;

    }

    /**
     * This method execute the narrow from the Helper and ior
     *
     * @param helperClass
     *            the helper
     * @param ior
     *            the reference to the object
     * @param orb
     *            the Orb reference
     *
     * @return Object narrowed Object
     */
    private static Object narrowObjbyIOR(Class helperClass, String ior, ORB orb)
            throws NoSuchMethodException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException {

        Object narrowedObj = helperClass.getDeclaredMethod("narrow",
                org.omg.CORBA.Object.class).invoke(helperClass,
                orb.string_to_object(ior));
        return narrowedObj;
    }

    /**
     * This Method Load the correct Helper for the narrowing of Corba Object
     * starting form the name of the interface
     *
     * @param String
     *            interfaceName
     * @param ChildFirstClassLoader
     *            originalClassLoader
     *
     * @return Class helper
     *
     */
    private static Class loadHelperbyName(String interfaceName,
            URLClassLoader originalClassLoader)
            throws ClassNotFoundException {

        return originalClassLoader.loadClass(interfaceName + "Helper");

    }

    /**
     *
     * @param enumObj
     * @param fromClassLoader
     * @param toClassLoader
     * @return
     * @throws SecurityException
     * @throws NoSuchMethodException
     * @throws ClassNotFoundException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     */
    private static Object transformEnumType(Object enumObj,
            URLClassLoader fromClassLoader, URLClassLoader toClassLoader)
            throws SecurityException, NoSuchMethodException,
            ClassNotFoundException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException {

        Class fromEnumClass = fromClassLoader.loadClass(TypeUtils.getTypeNameWithoutBrackets(enumObj.getClass()));
        Class toEnumClass = toClassLoader.loadClass(TypeUtils.getTypeNameWithoutBrackets(enumObj.getClass()));

        if (enumObj.getClass().isArray()) {
            int dimentions[] = TypeUtils.getObjectDimensions(enumObj);
            Object enumArray = Array.newInstance(toEnumClass, dimentions);
            Object[] fromEnumArray = (Object[]) enumObj;
            Object[] toEnumArray = (Object[]) enumArray;
            for (int i = 0; i < fromEnumArray.length; i++) {
                toEnumArray[i] = transformEnumType(fromEnumArray[i],
                        fromClassLoader, toClassLoader);
            }

            return toEnumArray;
        } else {

            Method valueMethod = fromEnumClass.getMethod("value");
            Object value = valueMethod.invoke(enumObj);

            Method from_intMethod = toEnumClass.getMethod("from_int", int.class);

            return from_intMethod.invoke(null, ((Integer) value).intValue());
        }
    }

    /**
     * getAllFields gets fields of class or inherited even if are not public
     *
     * @param cls
     * @param fields
     */
    private static void getAllFields(Class cls, List<Field> fields) {
        if (cls == null || cls.getName().startsWith("java.lang.")) {
            return;
        }

        Field[] clsFields = cls.getDeclaredFields();
        for (Field field : clsFields) {
            if (!ignoreField(field)) {
                fields.add(field);
            }
        }

        getAllFields(cls.getSuperclass(), fields);

    }

    /**
     * Ignore certain fields
     *
     * @param field
     * @return
     */
    private static boolean ignoreField(Field field) {
        int fieldMofidiers = field.getModifiers();

        //treat value type case(ignore field private static String[] _truncatable_ids)
        if (field.getName().equals("_truncatable_ids") && Modifier.isPrivate(fieldMofidiers) && Modifier.isStatic(fieldMofidiers)) {
            return true;
        }

        return false;
    }

    /**
     * getField gets field of class or inherited even if is not public
     *
     * @param cls
     * @param fieldName
     * @return
     */
    private static Field getField(Class cls, String fieldName) {
        Field[] clsFields = cls.getDeclaredFields();
        for (Field field : clsFields) {
            if (field.getName().equals(fieldName)) {
                return field;
            }
        }

        return getField(cls.getSuperclass(), fieldName);
    }

    /**
     * Gets the <code>MethodSignature</code> object from the <code>Method</code>
     * class. The correspondace between the MethodSignature and the method can
     * be saved for further optimization.
     *
     * @param method
     * @return
     */
    public static MethodSignature getMetodSignatureFromMethod(Method method, List<MethodSignature> methodSignatures) {

        MethodSignature methodSignature = null;
        for (int i = 0; i < methodSignatures.size(); i++) {
            methodSignature = (MethodSignature) methodSignatures.get(i);
            LOG.debug("METHOD SIGNATURE ===>" + methodSignature);
            if (compareMethodsWithNameAndParameters(method, methodSignature.getMethod())) {
                return methodSignature;
            }
        }
        String msg = MESSAGES.getString(
                "CRB000765_No_method_signature_found_for_method",
                new Object[]{method.toGenericString()});
        LOG.warn(msg);
        return null;
    }

    /**
     * Compares two method looking for the name and the parameters (not the
     * original class).
     *
     * @param methodA
     * @param methodB
     * @return
     */
    private static boolean compareMethodsWithNameAndParameters(Method methodA,
            Method methodB) {
        boolean ret = false;

        boolean paramEquals = compareParameterTypes(
                methodA.getParameterTypes(), methodB.getParameterTypes());

        if ((paramEquals) && (methodA.getName().equals(methodB.getName()))) {
            ret = true;
        }
        return ret;
    }

    /**
     * True if two <code>Class</code> array are of <code>Class</code> of the
     * same type.
     *
     * @param typesA
     * @param typesB
     * @return
     */
    private static boolean compareParameterTypes(Class[] typesA, Class[] typesB) {
        boolean ret = true;

        if (typesA.length != typesB.length) {
            ret = false;
        } else {
            for (int i = 0; i < typesA.length; i++) {
                if (!(typesA[i].getName().equals(typesB[i].getName()))) {

                    ret = false;
                    break;
                }
            }
        }
        return ret;
    }
            
    /**
     * Returns a multidimentional array from a linearized Object, see CRB-214.
     * @param objects
     * @param dimentions
     * @return
     */
    public static int  delinearizeArray(Object[] objects, Object myArray, int actualElement) {    
    	
    	LOG.debug("Delinearizing array called with array: " 
    			+ ArrayUtils.toString(myArray) + " and actualElement: " + actualElement);
    	
    	Object arrEl = myArray;  
    	int arrLen = Array.getLength(arrEl);
    	LOG.debug("myArray.getClass().getComponentType():" + myArray.getClass().getComponentType());
    	LOG.debug("arrEl:" + ArrayUtils.toString(arrEl));
    	LOG.debug("myArray.getClass().getComponentType().isArray:" + myArray.getClass().getComponentType().isArray());
    	if (myArray.getClass().getComponentType().isArray()) {
    		for (int k = 0; k < arrLen; k++) {
    			actualElement = delinearizeArray(objects, Array.get(arrEl, k) , actualElement);
    		}
    	} else {
    		for (int i = 0; i < arrLen; i++) {
    			LOG.debug("totIndex:" + actualElement + " Object: " +objects[actualElement]);                			
    			Array.set(myArray,i, objects[actualElement]);
    			actualElement ++;        			
    		}    		
    	}
    	return actualElement;
    }
   
        	
}
