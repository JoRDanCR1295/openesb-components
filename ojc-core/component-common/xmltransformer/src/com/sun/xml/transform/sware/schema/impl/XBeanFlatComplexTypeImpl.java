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
 * @(#)XBeanFlatComplexTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.QNameSet;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareElementWildcard;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType;
import com.sun.xml.transform.sware.schema.SwareGroup;
import com.sun.xml.transform.sware.schema.SwareParticle;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;


/**
 * @see com.sun.xml.transform.sware.schema.SwareFlatComplexType
 * @see com.sun.xml.transform.sware.schema.SwareGroup
 *  
 * @author Jun Xu
 * @version $Revision: 1.2 $
 */
class XBeanFlatComplexTypeImpl extends XBeanSpecificComponentImpl
        implements SwareFlatComplexType {

    private final XBeanSwareTypeSystemImpl mSchemaTypeSystem; 
    private final SchemaType mComplexType;
    
    private SwareComplexType mSwareComplexType;
    private Map<QName, AggregatedParticle> mAggregatedParticles;
    
    /**
     * Immediate child groups
     */
    private List<SchemaParticle> mChildGroups;
    private List<SwareGroup> mWrappedChildGroups;
    private SwareParticle[] mParticles;
    
    public Object mMonitor1 = new Object();
    public Object mMonitor2 = new Object();
    
    /**
     * Constructs from a complex type definition.
     * 
     * @param cType a complex type definition
     */
    XBeanFlatComplexTypeImpl(XBeanSwareTypeSystemImpl typeSystem,
            SchemaType cType) {
        mSchemaTypeSystem = typeSystem;
        mComplexType = cType;
    }

    /**
     * @see SwareFlatComplexType#getComplexType()
     */
    public SwareComplexType getComplexType() {
        if (mSwareComplexType != null) {
            return mSwareComplexType;
        }
        SwareComplexType cType;
        //To avoid compiler merging sync blocks and doing reorder, use
        //two different monitors
        synchronized (mMonitor1) {
            cType = new XBeanSwareComplexTypeImpl(mComplexType);
        }
        synchronized (mMonitor2) {
            if (mSwareComplexType != null) {
                return mSwareComplexType;
            }
            mSwareComplexType = cType;
        }
        return mSwareComplexType;
    }

    /**
     * @see SwareFlatComplexType#getChildGroups()
     */
    public List<SwareGroup> getChildGroups()
            throws SwareSchemaException  {
        
        getAggregatedParticles();
        
        if (mWrappedChildGroups != null && mParticles != null) {
            return mWrappedChildGroups;
        }
        List<SwareGroup> rootWrappedGroups;
        SwareParticle[] particles;
        List<SwareGroup> wrappedChildGroups;
        synchronized (mMonitor1) {
            rootWrappedGroups = new ArrayList<SwareGroup>();
            for (SchemaParticle group : mChildGroups) {
                rootWrappedGroups.add(new XBeanSwareGroupImpl(group));
            }
            
            wrappedChildGroups =
                Collections.unmodifiableList(rootWrappedGroups);
            particles = 
                new SwareParticle[wrappedChildGroups.size()];
            rootWrappedGroups.toArray(particles);
        }
        synchronized (mMonitor2) {
            if (mWrappedChildGroups != null && mParticles != null) {
                return mWrappedChildGroups;
            }
            mParticles = particles;
            mWrappedChildGroups = wrappedChildGroups;
        }
        return mWrappedChildGroups;
    }
    
    /**
     * @see SwareFlatComplexType#getAggregatedParticles(QName)
     */
    public AggregatedParticle getAggregatedParticles(QName qName)
            throws SwareSchemaException {
        return getAggregatedParticles().get(qName);
    }
    
    /**
     * @see SwareGroup#getCompositor()
     */
    public Compositor getCompositor() {
        return Compositor.SEQUENCE;
    }

    /**
     * @see SwareGroup#getParticles()
     */
    public SwareParticle[] getParticles() throws SwareSchemaException {
        getChildGroups();
        return mParticles;
    }

    /**
     * @see SwareGroup#isReference()
     */
    public boolean isReference() {
        return false;
    }

    /**
     * @see SwareGroup#getReference()
     */
    public SwareGroup getReference() {
        return null;
    }

    /**
     * @see SwareParticle#getParticleType()
     */
    public ParticleType getParticleType() {
        return ParticleType.GROUP;
    }

    /**
     * @see SwareParticle#getMinOccurs()
     */
    public BigInteger getMinOccurs() {
        return BigInteger.ONE;
    }

    /**
     * @see SwareParticle#getMaxOccurs()
     */
    public BigInteger getMaxOccurs() {
        return BigInteger.ONE;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        //No wrapped objects
        return null;
    }

    /**
     * @see SwareGroup#getParticleCount()
     */
    public int getParticleCount() throws SwareSchemaException {
        return getChildGroups().size();
    }

    /**
     * @see SwareGroup#getParticle(int)
     */
    public SwareParticle getParticle(int i) throws SwareSchemaException {
        return getChildGroups().get(i);
    }

    public ComponentType getComponentType() {
        return getComplexType().getComponentType();
    }

    public QName getName() {
        return getComplexType().getName();
    }

    public SwareElement getContainerElement() {
        return getComplexType().getContainerElement();
    }
    
    SwareTypeSystem getTypeSystem() {
        return mSchemaTypeSystem;
    }

    private Map<QName, AggregatedParticle>
            getAggregatedParticles() throws SwareSchemaException {
        if (mAggregatedParticles != null && mChildGroups != null) {
            return mAggregatedParticles;
        }
        Map<QName, AggregatedParticle> aggPartMap;
        List<SchemaParticle> rootGroups;
        synchronized (mMonitor1) {
            rootGroups = new ArrayList<SchemaParticle>();
            Map<QName, AggregatedParticleImpl> map =
                getAggregatedParticles(
                        mComplexType, new HashSet<SchemaType>(),
                        rootGroups);
            aggPartMap =
                Collections.unmodifiableMap(
                        new HashMap<QName, AggregatedParticle>(map));
            AggregatedWildcardImpl wildcard =
                (AggregatedWildcardImpl) aggPartMap.get(
                        SwareFlatComplexType.ELEMENT_WILDCARD_QNAME);
            if (wildcard != null) {
                for (AggregatedParticle aggPart
                        : aggPartMap.values()) {
                    if (aggPart.getParticleType().equals(
                            LeafParticleType.ELEMENTDECL)
                            && ((AggregatedElemDecl) aggPart).
                                hasNonOptionFlexibleAppearance()) {
                        if (wildcard.hasNonOptionFlexibleCoverage(
                                ((AggregatedElemDecl) aggPart).getName()
                                    .getNamespaceURI())) {
                            AggregatedParticleImpl aggPartImpl =
                                (AggregatedParticleImpl) aggPart;
                            aggPartImpl.setLocalDeterministic(false);
                            wildcard.setLocalDeterministic(false);
                        }
                    }
                }
            }
        }
        synchronized (mMonitor2) {
            if (mAggregatedParticles != null && mChildGroups != null) {
                return mAggregatedParticles;
            }
            mAggregatedParticles = aggPartMap;
            mChildGroups = Collections.unmodifiableList(rootGroups);
        }
        return mAggregatedParticles;
    }

    /**
     * Gets all elements and element wildcards from a complex type.
     * The key of the returned map is element's qualified name. For a wildcard,
     * the key is a QName with namespace equals
     * "http://www.w3.org/2001/XMLSchema" and local name equals "any".
     *
     * @param type an XML complex type definition
     * @param processedBaseTypes a set that contains all base types
     *                 that have been encountered while tries
     *                 to get all aggregated particles for the
     *                 complex type
     * @param rootGroups a list used to return all immediate child groups
     * @return a map that maps qualified names to aggregated particles 
     */
    private Map<QName, AggregatedParticleImpl> getAggregatedParticles(
            SchemaType type, Set<SchemaType> processedBaseTypes,
            List<SchemaParticle> rootGroups)
            throws SwareSchemaException {
        rootGroups.clear();
        Map<QName, AggregatedParticleImpl> thisMap =
            new HashMap<QName, AggregatedParticleImpl>();
        if (type.getContentType() == SchemaType.SIMPLE_CONTENT) {
            return thisMap;
        }
        if (processedBaseTypes.contains(type)) {
            //Circular derivation should be disallowed
            throw new SwareSchemaException(
                "Circular derivation is disallowed for complex type: '"
                    + type.getName() + "'");
        }

        if (type.getName() != null) {
            processedBaseTypes.add(type);
        }

        // DEVNOTE VM: With the current schema model we get the children particles including 
        // the particles defined in the base type in the case of extension elements. Hence we
        // no longer need to go into base type. Doing so only gives the wrong number of 
        // appearances for that particle and might result in an orderInderterministicException later on.
        // See OpenESB bug 1399 for details.
        // https://open-esb.dev.java.net/issues/show_bug.cgi?id=1399
        
//        Map<QName, AggregatedParticleImpl> baseMap = null;
//        SchemaType baseType = (SchemaType) type.getBaseType();
//        if (baseType != null
//                && type.getDerivationType() == SchemaType.DT_EXTENSION
//                && !baseType.isURType()) {
//            baseMap =
//                getAggregatedParticles(baseType, processedBaseTypes,
//                        rootGroups);
//        }

        SchemaParticle group = type.getContentModel();
        //Actually there will be at most one structure in the enumeration
        //and it should always be a Group
        if (group != null) {
            if (!isGroup(group)) {
                group = new FakeXBeanSchemaGroup(group);
            }
            ParticlePathImpl path = new ParticlePathImpl();
            path.addLevel(rootGroups.size());
            thisMap =
                getAggregatedParticles(group, path, group);
            rootGroups.add(group);
        }

//        if (baseMap != null) {
//            mergeAggregatedParticles(baseMap, thisMap);
//            return baseMap;
//        }
        
        return thisMap;
    }

    private Map<QName, AggregatedParticleImpl> getAggregatedParticles(
            SchemaParticle rootGroup, ParticlePathImpl path,
            SchemaParticle group)
            throws SwareSchemaException {
        
        Map<QName, AggregatedParticleImpl> thisMap =
            new HashMap<QName, AggregatedParticleImpl>();
        if (group.countOfParticleChild() == 0) {
            return thisMap;
        }

        int count = group.countOfParticleChild();
        for (int i = 0; i < count; i++) {
            ParticlePathImpl newPath = ParticlePathImpl.valueOf(path);
            newPath.addLevel(i);
            Map<QName, AggregatedParticleImpl> newMap =
                new HashMap<QName, AggregatedParticleImpl>();
            SchemaParticle part = group.getParticleChild(i);
            if ((part.getParticleType() == SchemaParticle.WILDCARD)
                    || (part.getParticleType() == SchemaParticle.ELEMENT)) {
                if (part.getParticleType() == SchemaParticle.WILDCARD) {
                    WildcardAppearanceImpl wdapp =
                        new WildcardAppearanceImpl(rootGroup, newPath, part);
                    AggregatedWildcardImpl aggwd =
                        new AggregatedWildcardImpl();
                    aggwd.addWildcardAppearance(wdapp);
                    newMap.put(ELEMENT_WILDCARD_QNAME, aggwd);
                } else {
                    SchemaLocalElement eDecl = (SchemaLocalElement) part;
                    ElemDeclAppearanceImpl elemApp =
                        new ElemDeclAppearanceImpl(rootGroup, newPath, eDecl);
                    AggregatedElemDeclImpl aggElem =
                        new AggregatedElemDeclImpl();
                    aggElem.addElemDeclAppearance(elemApp);
                    newMap.put(eDecl.getName(), aggElem);
                    Collection<QName> substs =
                        mSchemaTypeSystem.getSubstitutionSet(eDecl.getName());
                    if (substs != null) {
                        for (QName substQName : substs) {
                            SchemaGlobalElement substElemDecl =
                                mSchemaTypeSystem.xbeanFindElement(substQName);
                            if (substElemDecl == null) {
                                throw new SwareSchemaException(
                                        "Unable to find the element declaration"
                                        + " in the type system: " + substQName);
                            }
                            SubstituteAppearanceImpl substApp =
                                new SubstituteAppearanceImpl(rootGroup, newPath,
                                        elemApp, substElemDecl);
                            AggregatedElemDeclImpl aggElemSub =
                                new AggregatedElemDeclImpl();
                            aggElemSub.addSubstituteAppearance(substApp);
                            newMap.put(substQName, aggElemSub);
                        }
                    }
                }
            } else if (isGroup(part)) {
                newMap =
                    getAggregatedParticles(rootGroup, newPath, part);
            } else {
                throw new IllegalArgumentException(
                    "Unknown particle type"
                        + (part == null ? "null" : part.getParticleType()));
            }
            //merge to thisMap
            mergeAggregatedParticles(thisMap, newMap);
        }

        return thisMap;
    }

    private boolean isGroup(SchemaParticle part) {
        return part.getParticleType() == SchemaParticle.ALL
                || part.getParticleType() == SchemaParticle.SEQUENCE
                || part.getParticleType() == SchemaParticle.CHOICE;
    }
    
    private void mergeAggregatedParticles(
            Map<QName, AggregatedParticleImpl> mergeTo,
            Map<QName, AggregatedParticleImpl> mergeFrom) {
        for (Map.Entry<QName, AggregatedParticleImpl> entry
                : mergeFrom.entrySet()) {
            if (!mergeTo.containsKey(entry.getKey())) {
                mergeTo.put(entry.getKey(), entry.getValue());
            } else {
                AggregatedParticleImpl existing = mergeTo.get(entry.getKey());
                AggregatedParticleImpl newAggPart = entry.getValue();
                if (existing.getParticleType().equals(
                        LeafParticleType.ELEMENTDECL)) {
                    if (!newAggPart.getParticleType().equals(
                            LeafParticleType.ELEMENTDECL)) {
                        throw new IllegalStateException(
                                "New and existing particles must be"
                                + " of same type. existing type: "
                                + existing.getParticleType()
                                + ", new type: "
                                + newAggPart.getParticleType());
                    }
                    ((AggregatedElemDeclImpl) existing).addFrom(
                            (AggregatedElemDeclImpl) newAggPart);
                } else if (existing.getParticleType().equals(
                        LeafParticleType.WILDCARD)) {
                    if (!newAggPart.getParticleType().equals(
                            LeafParticleType.WILDCARD)) {
                        throw new IllegalStateException(
                                "New and existing particles must be"
                                + " of same type. existing type: "
                                + existing.getParticleType()
                                + ", new type: "
                                + newAggPart.getParticleType());
                    }
                    ((AggregatedWildcardImpl) existing).addFrom(
                            (AggregatedWildcardImpl) newAggPart);
                } else {
                    throw new IllegalArgumentException(
                            "Unknown particle type"
                            + existing.getParticleType());
                }
            }
        }
    }
    
    public String toString() {
        QName name;
        if ((name = getName()) != null) {
            return name.toString();
        }
        if (getContainerElement() != null) {
            if ((name = ((SwareElement) getContainerElement()).getName()) != null) {
                return "[local complex type of element: " + name + "]";
            }
        }
        return getComplexType().toString();
    }
    
    private static class ParticlePathImpl implements ParticlePath {
        private int mHashCode = 0;
        private List<Integer> mPaths = new ArrayList<Integer>();
        
        public static ParticlePathImpl valueOf(ParticlePathImpl path) {
            ParticlePathImpl newPath = new ParticlePathImpl();
            newPath.mPaths.addAll(path.mPaths);
            return newPath;
        }
        
        public int getNumOfLevels() {
            return mPaths.size();
        }
        
        public int getParticleIndex(int level) {
            if (level < 0 || level >= mPaths.size()) {
                throw new ArrayIndexOutOfBoundsException(level);
            }
            return mPaths.get(level);
        }
        
        public ParticlePath subpath(int startIndex, int len) {
            if (startIndex < 0 || startIndex >= mPaths.size()) {
                throw new ArrayIndexOutOfBoundsException(startIndex);
            }
            if (len < 0 || startIndex + len > mPaths.size()) {
                throw new IllegalArgumentException("Invalid len: " + len);
            }
            ParticlePathImpl newPath = new ParticlePathImpl();
            if (len == 0) {
                return newPath;
            }
            for (int i = startIndex; i < startIndex + len; i++) {
                newPath.mPaths.add(mPaths.get(i).intValue());
            }
            return newPath;
        }
        
        @Override
        public int hashCode() {
            int h = mHashCode;
            if (h == 0) {
                int len = mPaths.size();

                for (int i = 0; i < len; i++) {
                    h = 31 * h + mPaths.get(i);
                }
                mHashCode = h;
            }
            return h;
        }

        @Override
        public String toString() {
            if (mPaths.isEmpty()) {
                return "";
            }
            StringBuffer buff = new StringBuffer();
            for (int pi : mPaths) {
                if (buff.length() > 0) {
                    buff.append("/").append(pi);
                } else {
                    buff.append(pi);
                }
            }
            return buff.toString();
        }

        private void addLevel(int particleIndex) {
            mPaths.add(particleIndex);
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof ParticlePath)) {
                return false;
            }
            ParticlePath path = (ParticlePath) obj;
            if (mPaths.size() != path.getNumOfLevels()) {
                return false;
            }
            for (int i = 0; i < mPaths.size(); i++) {
                if (mPaths.get(i) != path.getParticleIndex(i)) {
                    return false;
                }
            }
            return true;
        }

        public int compareTo(ParticlePath o) {
            if (o == null) {
                return 1;
            }
            
            for (int i = 0; i < mPaths.size() && i < o.getNumOfLevels(); i++) {
                if (mPaths.get(i) < o.getParticleIndex(i)) {
                    return -1;
                } else if (mPaths.get(i) < o.getParticleIndex(i)) {
                    return 1;
                }
            }
            
            return 0;
        }
    }
    
    private static abstract class ParticleAppearanceImpl
            implements ParticleAppearance {
        
        private final LeafParticleType mParticleType;
        private final SchemaParticle mRootGroup;
        private SwareGroup mWrappedGroup;
        private ParticlePath mPath;
        private BigInteger mMinOccurs;
        private BigInteger mMaxOccurs;
        private boolean mOrderVerifiable = true;
        
        public Object mMonitor1 = new Object();
        public Object mMonitor2 = new Object();
        
        private ParticleAppearanceImpl(LeafParticleType particleType,
                SchemaParticle rootGroup, ParticlePath path) {
            if (rootGroup == null) {
                throw new NullPointerException("no root group.");
            }
            mParticleType = particleType;
            mRootGroup = rootGroup;
            mPath = path;
            SchemaParticle part = rootGroup;
            for (int i = 0; i < mPath.getNumOfLevels() - 1; i++) {
                if (part.getMaxOccurs() == null
                        || part.getMaxOccurs().compareTo(BigInteger.ONE) > 0) {
                    mOrderVerifiable = false;
                }
                part = part.getParticleChild(
                            mPath.getParticleIndex(i + 1));
            }
        }
        
        public boolean orderVerifiable() {
            return mOrderVerifiable;
        }

        public LeafParticleType getParticleType() {
            return mParticleType; 
        }
        
        public BigInteger getMinOccurs() {
            return mMinOccurs;
        }

        public BigInteger getMaxOccurs() {
            return mMaxOccurs;
        }

        public SwareGroup getRootGroup() {
            if (mWrappedGroup != null) {
                return mWrappedGroup;
            }
            SwareGroup wrap;
            //To avoid compiler merging sync blocks and doing reorder, use
            //two different monitors
            synchronized (mMonitor1) {
                wrap = new XBeanSwareGroupImpl(mRootGroup);
            }
            synchronized (mMonitor2) {
                if (mWrappedGroup != null) {
                    return mWrappedGroup;
                }
                mWrappedGroup = wrap;
            }
            return mWrappedGroup;
        }
        
        public ParticlePath getPath() {
            return mPath;
        }
        
        public abstract SwareParticle getParticle(); 
        
        protected void setMaxOccurs(int value) {
            if (value == -1) {
                mMaxOccurs = null;
            } else {
                mMaxOccurs = new BigInteger(String.valueOf(value));
            }
        }

        protected void setMinOccurs(int value) {
            mMinOccurs = new BigInteger(String.valueOf(value));
        }
    }
    
    private static class ElemDeclAppearanceImpl extends ParticleAppearanceImpl
            implements ElemDeclAppearance {
        
        private final SchemaLocalElement mElemDecl;
        
        private XBeanSwareElementImpl mWrappedElementDecl;
        
        private ElemDeclAppearanceImpl(SchemaParticle rootGroup,
                ParticlePath path, SchemaLocalElement elemDecl) {
            super(LeafParticleType.ELEMENTDECL, rootGroup, path);
            
            if (elemDecl == null) {
                throw new NullPointerException("no element declaration.");
            }
            mElemDecl = elemDecl;
            if (elemDecl.getMinOccurs() == null
                    && elemDecl.getMaxOccurs() == null) {
                //Must be an element that substitutes another element
                setMinOccurs(0);
                setMaxOccurs(0);
            } else {
                setMinOccurs(elemDecl.getMinOccurs().intValue());
                setMaxOccurs(elemDecl.getMaxOccurs() == null ? -1 
                        : elemDecl.getMaxOccurs().intValue());
            }
        }

        @Override
        public SwareParticle getParticle() {
            if (mWrappedElementDecl != null) {
                return mWrappedElementDecl;
            }
            XBeanSwareElementImpl wrap;
            //To avoid compiler merging sync blocks and doing reorder, use
            //two different monitors
            synchronized (mMonitor1) {
                wrap = new XBeanSwareElementImpl(mElemDecl);
            }
            synchronized (mMonitor2) {
                if (mWrappedElementDecl != null) {
                    return mWrappedElementDecl;
                }
                mWrappedElementDecl = wrap;
            }
            return mWrappedElementDecl;
        }
        
        public boolean isSubstitute() {
            return false;
        }
    }
    
    private static class WildcardAppearanceImpl extends ParticleAppearanceImpl
            implements WildcardAppearance {
        
        private final SchemaParticle mWildcard;
        private final QNameSet mQNameSet;
        private final Set<String> mIncluded;
        private final Set<String> mExcluded;
        
        private SwareElementWildcard mWrappedWildcard;
        
        private WildcardAppearanceImpl(SchemaParticle rootGroup,
                ParticlePath path, SchemaParticle wildcard) {
            super(LeafParticleType.WILDCARD, rootGroup, path);
            
            if (wildcard == null) {
                throw new NullPointerException("no wildcard.");
            }
            mWildcard = wildcard; 
            setMinOccurs(wildcard.getMinOccurs().intValue());
            setMaxOccurs(wildcard.getMaxOccurs() == null ? -1
                    : wildcard.getIntMaxOccurs());
            mQNameSet = wildcard.getWildcardSet();
            mIncluded = mQNameSet.includedURIs();
            mExcluded = mQNameSet.excludedURIs();
        }

        @Override
        public SwareParticle getParticle() {
            if (mWrappedWildcard != null) {
                return mWrappedWildcard;
            }
            SwareElementWildcard wrap;
            //To avoid compiler merging sync blocks and doing reorder, use
            //two different monitors
            synchronized (mMonitor1) {
                wrap = new XBeanSwareElementWildcardImpl(mWildcard);
            }
            synchronized (mMonitor2) {
                if (mWrappedWildcard != null) {
                    return mWrappedWildcard;
                }
                mWrappedWildcard = wrap;
            }
            return mWrappedWildcard;
        }

        public boolean coversNamespace(String nsURI) {
            if (mQNameSet.isAll()) {
                return true;
            }
            if (mIncluded != null && mIncluded.contains(nsURI)) {
                return true;
            }
            if (mExcluded != null && !mExcluded.contains(nsURI)) {
                return true;
            }
            return false;
        }

        private boolean hasOverlap(WildcardAppearanceImpl wdApp) {
            QNameSet interSet = mQNameSet.intersect(wdApp.mQNameSet);
            return !interSet.isEmpty();
        }
    }
    
    private static class SubstituteAppearanceImpl extends ElemDeclAppearanceImpl
            implements SubstituteAppearance {
        
        private final ElemDeclAppearance mSubstElemDeclAppearance;
        private final SchemaGlobalElement mElemDecl;
        private XBeanSwareElementImpl mWrappedElementDecl;
        
        private SubstituteAppearanceImpl(SchemaParticle rootGroup,
                ParticlePath path, ElemDeclAppearance substElemDeclAppearance,
                SchemaGlobalElement elemDecl) {
            super(rootGroup, path, elemDecl);
            
            mSubstElemDeclAppearance = substElemDeclAppearance;
            mElemDecl = elemDecl;
        }

        @Override
        public SwareParticle getParticle() {
            if (mWrappedElementDecl != null) {
                return mWrappedElementDecl;
            }
            XBeanSwareElementImpl wrap;
            synchronized (mMonitor1) {
                wrap = new XBeanSwareElementImpl(mElemDecl);
            }
            synchronized (mMonitor2) {
                if (mWrappedElementDecl != null) {
                    return mWrappedElementDecl;
                }
                mWrappedElementDecl = wrap;
            }
            return mWrappedElementDecl;
        }
        
        @Override
        public BigInteger getMinOccurs() {
            return mSubstElemDeclAppearance.getMinOccurs();
        }

        @Override
        public BigInteger getMaxOccurs() {
            return mSubstElemDeclAppearance.getMaxOccurs();
        }

        @Override
        public boolean isSubstitute() {
            return true;
        }
        
        public ElemDeclAppearance getSubstElemDeclAppearance() {
            return mSubstElemDeclAppearance;
        }
    }
    
    private static abstract class AggregatedParticleImpl
            implements AggregatedParticle {
        
        protected final Set<ParticleAppearance> mParticleAppearances =
            new LinkedHashSet<ParticleAppearance>();
        private final LeafParticleType mParticleType;
        protected Collection<ParticleAppearance> mSortedPartApps = null;
        
        private AggregatedParticleImpl(LeafParticleType particleType) {
            mParticleType = particleType;
        }
        
        public LeafParticleType getParticleType() {
            return mParticleType; 
        }
        
        public synchronized Collection<ParticleAppearance>
                getParticleAppearances() {
            if (mSortedPartApps != null) {
                return mSortedPartApps;
            }
            List<ParticleAppearance> list =
                new ArrayList<ParticleAppearance>();
            list.addAll(mParticleAppearances);
            Collections.sort(list,
                    new Comparator<ParticleAppearance>() {
                
                        public int compare(ParticleAppearance o1,
                                ParticleAppearance o2) {
                            return o2.getMinOccurs().compareTo(
                                    o1.getMinOccurs());
                        }
                    }
            );
            mSortedPartApps = Collections.unmodifiableCollection(list);
            return mSortedPartApps;
        }
        
        public abstract boolean isLocalDeterministic()
            throws SwareSchemaException;
        
        protected abstract void setLocalDeterministic(boolean value);
    }
    
    public static class AggregatedElemDeclImpl extends AggregatedParticleImpl
            implements AggregatedElemDecl {
        
        private boolean mDtmmLocalResultProduced;
        private boolean mDtmmLocalResult = true;
        private boolean mDtmmGlobalResult = true;
        
        private AggregatedElemDeclImpl() {
            super(LeafParticleType.ELEMENTDECL);
        }
        
        private void addFrom(AggregatedElemDeclImpl aggElem) {
            mParticleAppearances.addAll(aggElem.getParticleAppearances());
            mDtmmLocalResultProduced = false;
        }

        private void addElemDeclAppearance(ElemDeclAppearanceImpl elemApp) {
            mParticleAppearances.add(elemApp);
            mDtmmLocalResultProduced = false;
            mSortedPartApps = null;
        }

        private void addSubstituteAppearance(SubstituteAppearanceImpl substApp) {
            mParticleAppearances.add(substApp);
            mDtmmLocalResultProduced = false;
            mSortedPartApps = null;
        }

        @Override
        public synchronized boolean isLocalDeterministic()
                throws SwareSchemaException {
            if (!mDtmmLocalResultProduced) {
                Set<ParticlePath> dupCheckSet = new HashSet<ParticlePath>();
                boolean hasNonOptionalFlexibleOccurs = false;
                loopPartApps : for (ParticleAppearance partApp
                        : mParticleAppearances) {
                    if (!partApp.getMinOccurs().equals(partApp.getMaxOccurs())
                            && partApp.getMinOccurs().signum() > 0) {
                        if (!hasNonOptionalFlexibleOccurs) {
                            hasNonOptionalFlexibleOccurs = true;
                        } else {
                            mDtmmLocalResult = false;
                            mDtmmLocalResultProduced = true;
                            break;
                        }
                    }
                    ParticlePath path = partApp.getPath();
                    SwareParticle part = null;
                    boolean hasRepeatingGroup = false; 
                    for (int i = 0; i < path.getNumOfLevels(); i++) {
                        if (part == null) {
                            part = partApp.getRootGroup();
                        } else {
                            if (part.getParticleType().equals(
                                    ParticleType.GROUP)) {
                                SwareGroup group = (SwareGroup) part;
                                if (group.isReference()) {
                                    group = group.getReference();
                                }
                                part =
                                    group.getParticle(path.getParticleIndex(i));
                            }
                        }
                        if (hasRepeatingGroup) {
                            if (!part.getMinOccurs().equals(
                                    part.getMaxOccurs())
                                        && part.getMinOccurs().signum() > 0) {
                                mDtmmLocalResult = false;
                                mDtmmLocalResultProduced = true;
                                break loopPartApps;
                            }
                        }
                        if (part.getParticleType().equals(
                                ParticleType.GROUP)) {
                            if (part.getMaxOccurs() == null
                                    || part.getMaxOccurs().compareTo(
                                            BigInteger.ONE) > 0) {
                                hasRepeatingGroup = true;
                            }
                            if (((SwareGroup) part).getCompositor().equals(
                                    Compositor.CHOICE)) {
                                ParticlePath subPath = path.subpath(0, i + 1);
                                if (dupCheckSet.contains(subPath)) {
                                    mDtmmLocalResult = false;
                                    mDtmmLocalResultProduced = true;
                                    break loopPartApps;
                                } else {
                                    dupCheckSet.add(subPath);
                                }
                            }
                        }
                    }
                }
            }
            return mDtmmGlobalResult && mDtmmLocalResult;
        }

        public boolean hasNonOptionFlexibleAppearance() {
            for (ParticleAppearance partApp : mParticleAppearances) {
                if (!partApp.getMinOccurs().equals(partApp.getMaxOccurs())
                        && partApp.getMinOccurs().signum() > 0) {
                    return true;
                }
            }
            return false;
        }

        public QName getName() {
            if (mParticleAppearances.size() == 0) {
                return null;
            }
            ElemDeclAppearance elemApp =
                (ElemDeclAppearance) mParticleAppearances.iterator().next();
            return ((XBeanSwareElementImpl) elemApp.getParticle()).getName();
        }
        
        @Override
        protected void setLocalDeterministic(boolean value) {
            mDtmmGlobalResult = value;
        }
    }
    
    public static class AggregatedWildcardImpl extends AggregatedParticleImpl
            implements AggregatedWildcard {
        
        private boolean mDtmmLocalResultProduced;
        private boolean mDtmmLocalResult = true;
        private boolean mDtmmGlobalResult = true;
        private String mCandidateNamespace = null;
        private String mCandidatePartNamespace = null;
        private Collection<WildcardAppearance> mSortedCandidates = null;
        private Collection<ParticleAppearance> mSortedCandidateParticles = null;
        
        private AggregatedWildcardImpl() {
            super(LeafParticleType.WILDCARD);
        }

        private void addFrom(AggregatedWildcardImpl aggWd) {
            mParticleAppearances.addAll(aggWd.getParticleAppearances());
            mDtmmLocalResultProduced = false;
            mSortedPartApps = null;
            mCandidateNamespace = null;
            mSortedCandidates = null;
            mCandidatePartNamespace = null;
            mSortedCandidateParticles = null;
        }

        private void addWildcardAppearance(WildcardAppearanceImpl wdApp) {
            mParticleAppearances.add(wdApp);
            mDtmmLocalResultProduced = false;
            mSortedPartApps = null;
            mCandidateNamespace = null;
            mSortedCandidates = null;
            mCandidatePartNamespace = null;
            mSortedCandidateParticles = null;
        }

        @Override
        public synchronized boolean isLocalDeterministic()
                throws SwareSchemaException {
            if (!mDtmmLocalResultProduced) {
                List<WildcardAppearanceImpl> nonOptionalFlexibleWdApps
                    = new ArrayList<WildcardAppearanceImpl>();
                Map<ParticlePath, WildcardAppearanceImpl> dupCheckMap =
                    new HashMap<ParticlePath, WildcardAppearanceImpl>();
                loopPartApps : for (ParticleAppearance partApp
                        : mParticleAppearances) {
                    if (!partApp.getMinOccurs().equals(partApp.getMaxOccurs())
                            && partApp.getMinOccurs().signum() > 0) {
                        for (WildcardAppearanceImpl nofWdApp :
                            nonOptionalFlexibleWdApps) {
                            if (nofWdApp.hasOverlap(
                                    (WildcardAppearanceImpl) partApp)) {
                                mDtmmLocalResult = false;
                                mDtmmLocalResultProduced = true;
                                break loopPartApps;
                            }
                        }
                        nonOptionalFlexibleWdApps.add(
                                (WildcardAppearanceImpl) partApp);
                    }
                    ParticlePath path = partApp.getPath();
                    SwareParticle part = null;
                    boolean hasRepeatingGroup = false; 
                    for (int i = 0; i < path.getNumOfLevels(); i++) {
                        if (part == null) {
                            part = partApp.getRootGroup();
                        } else {
                            if (part.getParticleType().equals(
                                    ParticleType.GROUP)) {
                                SwareGroup group = (SwareGroup) part;
                                if (group.isReference()) {
                                    group = group.getReference();
                                }
                                part =
                                    group.getParticle(path.getParticleIndex(i));
                            }
                        }
                        if (hasRepeatingGroup) {
                            if (!part.getMinOccurs().equals(
                                    part.getMaxOccurs())
                                        && part.getMinOccurs().signum() > 0) {
                                mDtmmLocalResult = false;
                                mDtmmLocalResultProduced = true;
                                break loopPartApps;
                            }
                        }
                        if (part.getParticleType().equals(
                                ParticleType.GROUP)) {
                            if (part.getMaxOccurs() == null
                                    || part.getMaxOccurs().compareTo(
                                            BigInteger.ONE) > 0) {
                                hasRepeatingGroup = true;
                            }
                            if (((SwareGroup) part).getCompositor().equals(
                                    Compositor.CHOICE)) {
                                ParticlePath subPath = path.subpath(0, i + 1);
                                if (dupCheckMap.containsKey(subPath)
                                        && dupCheckMap.get(subPath).hasOverlap(
                                     (WildcardAppearanceImpl) partApp)) {
                                    mDtmmLocalResult = false;
                                    mDtmmLocalResultProduced = true;
                                    break loopPartApps;
                                } else {
                                    dupCheckMap.put(subPath,
                                            (WildcardAppearanceImpl) partApp);
                                }
                            }
                        }
                    }
                }
            }
            return mDtmmGlobalResult && mDtmmLocalResult;
        }

        public boolean coversNamespace(String nsURI) {
            for (ParticleAppearance partApp : mParticleAppearances) {
                if (((WildcardAppearance) partApp).coversNamespace(nsURI)) {
                    return true;
                }
            }
            return false;
        }

        public synchronized Collection<WildcardAppearance>
                getCandidates(String nsURI) {
            if (mSortedCandidates != null
                    && Util.sameNamespace(nsURI, mCandidateNamespace)) {
                return mSortedCandidates;
            }
            List<WildcardAppearance> canList =
                new ArrayList<WildcardAppearance>();
            for (ParticleAppearance partApp : getParticleAppearances()) {
                if (((WildcardAppearance) partApp).coversNamespace(nsURI)) {
                    canList.add((WildcardAppearance) partApp);
                }
            }
            mCandidateNamespace = nsURI;
            mSortedCandidates = canList;
            return mSortedCandidates;
        }

        public synchronized Collection<ParticleAppearance>
                getCandidateParticles(String nsURI) {
            if (mSortedCandidateParticles != null
                    && Util.sameNamespace(nsURI, mCandidatePartNamespace)) {
                return mSortedCandidateParticles;
            }
            List<ParticleAppearance> canList =
                new ArrayList<ParticleAppearance>();
            for (ParticleAppearance partApp : getParticleAppearances()) {
                if (((WildcardAppearance) partApp).coversNamespace(nsURI)) {
                    canList.add((WildcardAppearance) partApp);
                }
            }
            mCandidatePartNamespace = nsURI;
            mSortedCandidateParticles = canList;
            return mSortedCandidateParticles;
        }

        @Override
        protected synchronized void setLocalDeterministic(boolean value) {
            mDtmmGlobalResult = value;
        }

        public boolean hasNonOptionFlexibleCoverage(String nsURI) {
            for (ParticleAppearance partApp : mParticleAppearances) {
                WildcardAppearance wdApp = (WildcardAppearance) partApp;
                if (wdApp.coversNamespace(nsURI)
                        && !wdApp.getMinOccurs().equals(wdApp.getMaxOccurs())
                        && wdApp.getMinOccurs().signum() > 0) {
                    return true;
                }
            }
            return false;
        }
    }
}
