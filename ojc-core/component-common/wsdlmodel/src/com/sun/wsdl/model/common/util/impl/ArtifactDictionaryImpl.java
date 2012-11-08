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
 * @(#)ArtifactDictionaryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.util.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.sun.wsdl.model.common.util.ArtifactDictionary;
import com.sun.wsdl.model.common.util.Utility;

/**
 * Implements dictonary of artifacts.
 * @author Sun Microsystems
 * @version 
 */
public class ArtifactDictionaryImpl implements ArtifactDictionary {
    
    /** Holds the map */
    protected Map mMap = null;
    
    /** Holds reference to eInsightManager */

    
    /** Creates a new instance of ArtifactDictionaryImpl */
    public ArtifactDictionaryImpl() {
        super();
        
        mMap = new HashMap();
    }
    
    /** @see EInsightManagerCreatable#initializeEInsightManager
     */
    
    /** Tests whether two Classes are equal.
     * @param   cls1    First class (either exactly equal to this or is <code>instanceof</code> this).
     * @param   cls2    Second class
     * @return  <code>true</code> if equal
     */
    protected boolean classEquals(Class cls1, Class cls2) {
        return ((cls1 == cls2) || cls1.isAssignableFrom(cls2) || cls2.isAssignableFrom(cls1));
    }
    
    /** @see ArtifactDictionary#putAll
     */
    public void putAll(ArtifactDictionary dict) {
        mMap.putAll(((ArtifactDictionaryImpl) dict).mMap);
    }
    
    /** @see ArtifactDictionary#put(String, Class, String, Object)
     */
    public Object put(String ns, Class cls, String name, Object artifact) {
        if (Utility.isEmpty(ns) || (null == cls) || Utility.isEmpty(name) || (null == artifact)) {
            return null;
        }
        return mMap.put(new ArtifactKey(ns, cls, name), artifact);
    }

    /** @see ArtifactDictionary#get(String, Class, String)
     */
    public Object get(String ns, Class cls, String name) {
        if (Utility.isEmpty(ns) || (null == cls) || Utility.isEmpty(name)) {
            return null;
        }
        
        return mMap.get(new ArtifactKey(ns, cls, name));
    }
    
    /** @see ArtifactDictionary#get()
     */
    public Collection get() {
        Collection rColl = new ArrayList(mMap.size());
        for (Iterator iter = mMap.values().iterator(); iter.hasNext();) {
            rColl.add(iter.next());
        }
        return rColl;
    }
    
    /** @see ArtifactDictionary#get(String)
     */
    public Collection get(String ns) {
        Collection rColl = new ArrayList();
        if (Utility.isEmpty(ns)) {
            return rColl;
        }
        
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            ArtifactKey artKey = (ArtifactKey) me.getKey();
            if (ns.equals(artKey.getNamespace())) {
                rColl.add(me.getValue());
            }
        }
        return rColl;
    }
    
    /** @see ArtifactDictionary#get(String, Class)
     */
    public Collection get(String ns, Class cls) {
        Collection rColl = new ArrayList();
        if (Utility.isEmpty(ns) || (null == cls)) {
            return rColl;
        }
        
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            ArtifactKey artKey = (ArtifactKey) me.getKey();
            if (ns.equals(artKey.getNamespace()) && classEquals(cls, artKey.getType())) {
                rColl.add(me.getValue());
            }
        }
        return rColl;
    }
    
    /** @see ArtifactDictionary#get(Class)
     */
    public Collection get(Class cls) {
        Collection rColl = new ArrayList();
        if (null == cls) {
            return rColl;
        }
        
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            ArtifactKey artKey = (ArtifactKey) me.getKey();
            if (classEquals(cls, artKey.getType())) {
                rColl.add(me.getValue());
            }
        }
        return rColl;
    }
    
    /** @see ArtifactDictionary#getByName
     */
    public Collection getByName(String name) {
        Collection rColl = new ArrayList();
        if (Utility.isEmpty(name)) {
            return rColl;
        }
        
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            ArtifactKey artKey = (ArtifactKey) me.getKey();
            if (name.equals(artKey.getName())) {
                rColl.add(me.getValue());
            }
        }
        return rColl;
    }
    
    /** @see ArtifactDictionary#getNamespaceKeys
     */
    public Set getNamespaceKeys() {
        Set rSet = new HashSet();
        for (Iterator iter = mMap.keySet().iterator(); iter.hasNext();) {
            ArtifactKey artKey = (ArtifactKey) iter.next();
            rSet.add(artKey.getNamespace());
        }
        return rSet;
    }
    
    /** @see ArtifactDictionary#getTypeKeys
     */
    public Set getTypeKeys() {
        Set rSet = new HashSet();
        for (Iterator iter = mMap.keySet().iterator(); iter.hasNext();) {
            ArtifactKey artKey = (ArtifactKey) iter.next();
            rSet.add(artKey.getType());
        }
        return rSet;
    }
    
    /** @see ArtifactDictionary#getNameKeys()
     */
    public Set getNameKeys() {
        Set rSet = new HashSet();
        for (Iterator iter = mMap.keySet().iterator(); iter.hasNext();) {
            ArtifactKey artKey = (ArtifactKey) iter.next();
            rSet.add(artKey.getName());
        }
        return rSet;
    }
    
    /** @see Object#toString
     */
    public String toString() {
        StringBuffer sb = new StringBuffer("{");
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            String key = me.getKey().toString();
            Object artifact = me.getValue();
            sb.append(key).append("=")
                .append((this == artifact) ? "(this ArtifactDictionary)" : artifact.toString());
            if (iter.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    /** @see ArtifactDictionary#getNamespaceKeys(Class)
     */
    public Set getNamespaceKeys(Class cls) {
        Set rSet = new HashSet();
        if (null == cls) {
            return rSet;
        }
        
        for (Iterator iter = mMap.entrySet().iterator(); iter.hasNext();) {
            Map.Entry me = (Map.Entry) iter.next();
            ArtifactKey artKey = (ArtifactKey) me.getKey();
            if (classEquals(cls, artKey.getType())) {
                rSet.add(artKey.getNamespace());
            }
        }
        return rSet;
    }
    
    /** @see ArtifactDictionary#size
     */
    public int size() {
        return mMap.size();
    }
    
    /** @see ArtifactDictionary#isEmpty
     */
    public boolean isEmpty() {
        return mMap.isEmpty();
    }
    
    /** Class to represent the key for the Artifact Dictionary
     */
    public static class ArtifactKey {

        /** Delimiter between fields in key */
        protected static final String DELIM = "|";
        
        /** Holds the namespace */
        String mNamespace;
        
        /** Holds the artifact type */
        Class mType;
        
        /** Holds the artifact name */
        String mName;
        
        /** Constructs a key.
         * @param   namespace   Namespace of artifact.
         * @param   type        Type (Class) of artifact.
         * @param   name        Name of artifact.
         */
        ArtifactKey(String namespace, Class type, String name) {
            super();
            mNamespace = namespace;
            mType = type;
            mName = name;
        }
        
        /** Gets the namespace.
         * @return  Namespace of artifact.
         */
        public String getNamespace() {
            return mNamespace;
        }
        
        /** Gets the type.
         * @return  Type (Class) of artifact.
         */
        public Class getType() {
            return mType;
        }
        
        /** Gets the name.
         * @return  Name of artifact.
         */
        public String getName() {
            return mName;
        }
        
        /** @see java.lang.Object#hashCode
         */
        public int hashCode() {
            return mNamespace.hashCode() + mType.hashCode() + mName.hashCode();
        }

        /** Tests whether two Classes are equal.
         * @param   cls1    First class (either exactly equal to this or is <code>instanceof</code> this).
         * @param   cls2    Second class
         * @return  <code>true</code> if equal
         */
        protected boolean classEquals(Class cls1, Class cls2) {
            return ((cls1 == cls2) || cls1.isAssignableFrom(cls2) || cls2.isAssignableFrom(cls1));
        }
        
        /** @see java.lang.Object#equals
         */
        public boolean equals(Object artKey) {
            return ((this == artKey) || ((artKey instanceof ArtifactKey)
                                         && getNamespace().equals(((ArtifactKey) artKey).getNamespace())
                                         && classEquals(getType(), ((ArtifactKey) artKey).getType())
                                         && getName().equals(((ArtifactKey) artKey).getName())));
        }
        
        /** @see java.lang.Object#toString
         */
        public String toString() {
            return (getNamespace() + DELIM + getType() + DELIM + getName());
        }
    }
}
