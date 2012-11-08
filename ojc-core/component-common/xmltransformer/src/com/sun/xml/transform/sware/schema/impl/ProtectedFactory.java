/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ProtectedFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import java.lang.ref.SoftReference;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.sun.xml.transform.sware.schema.ImplementationType;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * This factory class provides another level of indirection for creating
 * instances of schema type system and creating wrapped schema objects etc.
 * With this class in place, the public factory classes do not need to deal
 * directly with the constructors of specific implementation classes.
 * 
 * @author Jun Xu
 * @see com.sun.xml.transform.sware.schema.SwareTypeSystem.Factory
 * @see com.sun.xml.transform.sware.schema.SwareSchema.Factory
 * @version $Revision: 1.4 $ 
 */
public final class ProtectedFactory {
    
    static Map<Object, SoftReference<SwareSchema>> mSchemaWriteCache =
        new HashMap<Object, SoftReference<SwareSchema>>();
    static Map<Object, SoftReference<SwareSchema>> mSchemaReadCache =
        new HashMap<Object, SoftReference<SwareSchema>>();
    
    static Class mCastorSchemaClass;
    static Class mXmlBeansSchemaDocClass;
    static Class mXmlBeansSchemaTypeLoaderClass;
    
    static {
        try {
            mCastorSchemaClass =
                Class.forName("org.exolab.castor.xml.schema.Schema");
        } catch (ClassNotFoundException e) {
            //Ignore.  Leave mCastorSchemaClass null
        }
        try {
            mXmlBeansSchemaDocClass =
                Class.forName(
                        "org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument");
        } catch (ClassNotFoundException e) {
            //Ignore.  Leave mXmlBeansSchemaDocClass null
        }
        try {
            mXmlBeansSchemaTypeLoaderClass =
                Class.forName(
                        "org.apache.xmlbeans.SchemaTypeLoader");
        } catch (ClassNotFoundException e) {
            //Ignore.  Leave mXmlBeansSchemaTypeLoaderClass null
        }
    }

    /**
     * Creates a schema type system based in a specific implementation type.
     * 
     * @param implType a specific implementation type
     * @return a schema type system instance
     * @see com.sun.xml.transform.sware.schema.SwareTypeSystem.Factory#getSchemaTypeSystem(ImplementationType)
     */
    public static SwareTypeSystem newSwareTypeSystem(
            ImplementationType implType) {
        if (isCastorAvailable() && implType.equals(ImplementationType.CASTOR)) {
            SwareTypeSystem ts;
            try {
                //Loads instance using reflection so this class will not
                //have dependency on Castor specific classes 
                ts = (SwareTypeSystem)
                    Class.forName("com.sun.xml.transform.sware.schema.impl.CastorSwareTypeSystemImpl").newInstance();
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            }
            return ts;
        } else if (isXmlBeansAvailable() && implType.equals(ImplementationType.XMLBEANS)) {
            SwareTypeSystem ts;
            try {
                //Loads instance using reflection so this class will not
                //have dependency on XmlBeans specific classes 
                ts = (SwareTypeSystem)
                    Class.forName("com.sun.xml.transform.sware.schema.impl.XBeanSwareTypeSystemImpl").newInstance();
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            }
            return ts;
        } else {
            throw new UnsupportedOperationException(
                    "Unrecognized implementation type: " + implType);
        }
    }
    
    public static SwareTypeSystem newSwareTypeSystem(
            ImplementationType implType, Object implSpecficTS) {
        if (isCastorAvailable() && implType.equals(ImplementationType.CASTOR)) {
            SwareTypeSystem ts;
            try {
                //Loads instance using reflection so this class will not
                //have dependency on Castor specific classes 
                Class clazz = Class.forName(
                        "com.sun.xml.transform.sware.schema.impl.CastorSwareTypeSystemImpl");
                Constructor cons = clazz.getConstructor(Set.class);
                ts = (SwareTypeSystem) cons.newInstance(implSpecficTS);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            } catch (SecurityException e) {
                throw new RuntimeException("SecurityException", e);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("NoSuchMethodException", e);
            } catch (IllegalArgumentException e) {
                throw new RuntimeException("IllegalArgumentException", e);
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException("InvocationTargetException", e);
            }
            return ts;
        } else if (isXmlBeansAvailable() && implType.equals(ImplementationType.XMLBEANS)) {
            SwareTypeSystem ts;
            try {
                //Loads instance using reflection so this class will not
                //have dependency on XmlBeans specific classes 
                Class clazz = Class.forName(
                "com.sun.xml.transform.sware.schema.impl.XBeanSwareTypeSystemImpl");
                Constructor cons = clazz.getConstructor(mXmlBeansSchemaTypeLoaderClass);
                ts = (SwareTypeSystem) cons.newInstance(implSpecficTS);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            } catch (SecurityException e) {
                throw new RuntimeException("SecurityException", e);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("NoSuchMethodException", e);
            } catch (IllegalArgumentException e) {
                throw new RuntimeException("IllegalArgumentException", e);
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException("InvocationTargetException", e);
            }
            return ts;
        } else {
            throw new UnsupportedOperationException(
                    "Unrecognized implementation type: " + implType);
        }
    }
    
    /**
     * Gets the wrapped schema object from an implementation specific schema
     * object.
     * 
     * @param implSpecificSchemaObject an implemenation specific schema object.
     *          For Castor it should be an intance of
     *          <code>org.exolab.castor.xml.schema.Schema</code>.
     *          For XmlBeans it should be an instance of
     *          <code>org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument</code>
     *          or
     *          <code>org.apache.xmlbeans.impl.xb.xsdschema.Schema</code>.
     * @return a wrapped schema object that implements the SwareSchema interface
     * @see SwareSchema.Factory#getSwareSchema(Object)
     */
    public static SwareSchema getSwareSchema(Object implSpecificSchemaObject) {
        if (isCastorAvailable() && isCastorSchema(implSpecificSchemaObject)) {
            SoftReference<SwareSchema> ref =
                mSchemaReadCache.get(implSpecificSchemaObject);
            SwareSchema swareSchema;
            if (ref != null && (swareSchema = ref.get()) != null) {
                return swareSchema;
            }
            try {
                //Loads instance using reflection so this class will not
                //have dependency on Castor specific classes 
                Class clazz = Class.forName("com.sun.xml.transform.sware.schema.impl.CastorSwareSchemaImpl");
                Constructor cons = clazz.getConstructor(Object.class);
                swareSchema =
                    (SwareSchema) cons.newInstance(implSpecificSchemaObject);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            } catch (SecurityException e) {
                throw new RuntimeException("SecurityException", e);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("NoSuchMethodException", e);
            } catch (IllegalArgumentException e) {
                throw new RuntimeException("IllegalArgumentException", e);
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException("InvocationTargetException", e);
            }
            synchronized(mSchemaWriteCache){
                if (!mSchemaWriteCache.containsKey(implSpecificSchemaObject)) {
                    mSchemaWriteCache.put(implSpecificSchemaObject,
                            new SoftReference<SwareSchema>(swareSchema));
                    mSchemaReadCache =
                        new HashMap<Object, SoftReference<SwareSchema>>(
                                mSchemaWriteCache);
                }
            }
            return swareSchema;
        } else if (isXmlBeansAvailable() && isXmlBeansSchema(implSpecificSchemaObject)) {
            SoftReference<SwareSchema> ref =
                mSchemaReadCache.get(implSpecificSchemaObject);
            SwareSchema swareSchema;
            if (ref != null && (swareSchema = ref.get()) != null) {
                    return swareSchema;
            }
            try {
                //Loads instance using reflection so this class will not
                //have dependency on XmlBeans specific classes 
                Class clazz = Class.forName("com.sun.xml.transform.sware.schema.impl.XBeanSwareSchemaImpl");
                Constructor cons = clazz.getConstructor(Object.class);
                swareSchema =
                    (SwareSchema) cons.newInstance(implSpecificSchemaObject);
            } catch (ClassNotFoundException e) {
                throw new RuntimeException("ClassNotFoundException", e);
            } catch (SecurityException e) {
                throw new RuntimeException("SecurityException", e);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException("NoSuchMethodException", e);
            } catch (IllegalArgumentException e) {
                throw new RuntimeException("IllegalArgumentException", e);
            } catch (InstantiationException e) {
                throw new RuntimeException("InstantiationException", e);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("IllegalAccessException", e);
            } catch (InvocationTargetException e) {
                throw new RuntimeException("InvocationTargetException", e);
            }
            synchronized(mSchemaWriteCache){
                if (!mSchemaWriteCache.containsKey(implSpecificSchemaObject)) {
                    mSchemaWriteCache.put(implSpecificSchemaObject,
                            new SoftReference<SwareSchema>(swareSchema));
                    mSchemaReadCache =
                        new HashMap<Object, SoftReference<SwareSchema>>(
                                mSchemaWriteCache);
                }
            }
            return swareSchema;
        } else {
            throw new UnsupportedOperationException(
                    "Wrapping support is not available for the schema object.");
        }
    }
    
    private static boolean isCastorAvailable() {
        return mCastorSchemaClass != null;
    }
    
    private static boolean isXmlBeansAvailable() {
        return mXmlBeansSchemaTypeLoaderClass != null;
    }
    
    private static boolean isCastorSchema(Object schema) {
        if (!isCastorAvailable()) {
            throw new UnsupportedOperationException("No Castor support.");
        }
        return mCastorSchemaClass.isInstance(schema);
    }
    
    private static boolean isXmlBeansSchema(Object schema) {
        if (!isXmlBeansAvailable()) {
            throw new UnsupportedOperationException("No XmlBeans support.");
        }
        return mXmlBeansSchemaDocClass.isInstance(schema);
    }
}
