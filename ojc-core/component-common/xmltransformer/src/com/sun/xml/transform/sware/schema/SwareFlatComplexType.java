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
 * @(#)SwareFlatComplexType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

/**
 * The instance of this interface represents a complex type definition with a
 * flattened particle structure (all groups/model groups are removed and all
 * particles with same qualified name are aggregated).  Flattened complex type
 * facilitates sorting.
 *   
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareFlatComplexType extends SwareGroup {

    /**
     * Enumeration type for leaf particles (non-group).
     */
    public enum LeafParticleType {
        ELEMENTDECL,
        WILDCARD
    }
    
    /**
     * A special qualified name representing wildcard.  Hope it will not
     * conflict with any real qualified names.
     */
    public static final QName ELEMENT_WILDCARD_QNAME =
        new QName("____any____1650640323339983851");
    
    /**
     * Gets the original unflattened complex type.
     * 
     * @return the original unflattened complex type.
     */
    public SwareComplexType getComplexType();

    /**
     * Gets an aggregated particle using a qualified name.
     * 
     * @param qName the qualified name
     * @return an aggregated particle that matches the qualified name
     * @throws SwareSchemaException invalid schema exception
     */
    public AggregatedParticle getAggregatedParticles(QName qName) 
            throws SwareSchemaException;
    
    /**
     * gets child model groups (only first level groups, not including
     * recursive ones)
     * 
     * @return a list of model groups
     * @throws SwareSchemaException invalid schema exception
     */
    public List<SwareGroup> getChildGroups()
            throws SwareSchemaException;
    
    /**
     * Gets the container element of this complex type.
     * 
     * @return the container element.
     */
    public SwareElement getContainerElement();
    
    /**
     * The instance of this class represents a particle path, which can be used
     * to reach this partcile from the complex type along model groups.
     */
    public interface ParticlePath extends Comparable<ParticlePath> {
        public int getNumOfLevels();
        
        public int getParticleIndex(int level);
        
        public ParticlePath subpath(int startIndex, int len);
    }

    /**
     * The instance of this class represents one particle appearance.  Same
     * particle might appear in a complex type multiple times.
     */
    public interface ParticleAppearance {
        
        /**
         * Gets the particle type of this particle.
         * 
         * @return particle type
         */
        public LeafParticleType getParticleType();
        
        /**
         * Gets the minOccurs of the particle.
         * 
         * @return minOccurs
         */
        public BigInteger getMinOccurs();

        /**
         * Gets the maxOccurs of the particle.
         * 
         * @return maxOccurs of the particle
         */
        public BigInteger getMaxOccurs();

        /**
         * Gets the root group (the model group right below a complex type
         * definition) the particle belongs to.
         * 
         * @return the root group
         */
        public SwareGroup getRootGroup();
        
        /**
         * Gets the particle path of this appearance.
         * 
         * @return the particle path of this appearance.
         */
        public ParticlePath getPath();
        
        /**
         * Gets the particle definition/declaration.
         *  
         * @return particle definition/declaration
         */
        public SwareParticle getParticle();
        
        /**
         * Tests if the order of element that matches this particle can be
         * verified.
         * 
         * @return <code>true</code> if the order can be verified.
         */
        public boolean orderVerifiable();
    }
    
    /**
     * Element particle appearance.
     */
    public interface ElemDeclAppearance extends ParticleAppearance {
        
        public boolean isSubstitute();
    }
    
    /**
     * Element wildcard particle appearance.
     */
    public interface WildcardAppearance extends ParticleAppearance {
        
        /**
         * Tests if the specified namespace is covered by this wildcard.
         * 
         * @param nsURI a namespace URI
         * @return <code>true</code> if the namespace is covered by this
         *         wildcard, otherwise <code>false</code>
         */
        public boolean coversNamespace(String nsURI);
    }
    
    /**
     * Substitution element particle appearance.
     */
    public interface SubstituteAppearance extends ElemDeclAppearance {
        
        public ElemDeclAppearance getSubstElemDeclAppearance();
    }
    
    /**
     * The generic aggregated particle interface.
     */
    public interface AggregatedParticle {

        /**
         * Gets the particle type.
         * 
         * @return particle type
         */
        public LeafParticleType getParticleType();
        
        /**
         * Gets all particle appearences.
         * 
         * @return an immutable collection of particle appearances
         */
        public Collection<ParticleAppearance> getParticleAppearances();
        
        /**
         * Tests if the sorting order is locally deterministic for this
         * particle appearance.
         * 
         * @return <code>true</code> if is deterministic, otherwise
         *         <code>false<code>
         */
        public boolean isLocalDeterministic() throws SwareSchemaException;
    }
    
    /**
     * Aggregated element particle.
     */
    public interface AggregatedElemDecl extends AggregatedParticle {
        
        /**
         * Tests if there is non-optional flexible element appearance in this
         * aggregated element declaration.
         * 
         * @return <code>true</code> if there is non-optional flexible element
         *          appearance in this aggregated element declaration,
         *          otherwise <code>false</code>
         */
        public boolean hasNonOptionFlexibleAppearance();
        
        /**
         * Gets the qualified name of the element declaration.  It can be
         * the name of any element declaration appearance inside this
         * aggregated element declaration since they are all the same.
         * 
         * @return the qualified name of the element declaration.  Might be
         *         <code>null</code> if there is no element declaration
         *         appearance.
         */
        public QName getName();
    }
    
    /**
     * Aggregated element wildcard particle.
     */
    public interface AggregatedWildcard extends AggregatedParticle {

        /**
         * Tests if the specified namespace is covered by this wildcard.
         * 
         * @param nsURI a namespace URI
         * @return <code>true</code> if the namespace is covered by this
         *         wildcard, otherwise <code>false</code>
         */
        public boolean coversNamespace(String nsURI);
        
        /**
         * Tests if the specified namespace is covered by wildcard appearances
         * that are non-optional but with flexible occurrences.
         * 
         * @param nsURI a namespace URI
         * @return <code>true</code> if the namespace is covered by this
         *         wildcard, otherwise <code>false</code>
         */
        public boolean hasNonOptionFlexibleCoverage(String nsURI);
        
        /**
         * Gets a list of wildcard appearance instances that cover
         * the specified namespace.
         * 
         * @param nsURI a namespace URI
         * @return a collection of wildcard appearance instances that cover
         *         the specified namespace
         */
        public Collection<WildcardAppearance> getCandidates(String nsURI);
        
        /**
         * Gets a list of wildcard appearance instances that cover
         * the specified namespace.  This method is same as
         * <code>getCandidates</code> except it returns a collection
         * of ParticleAppearance instances.
         * 
         * @param nsURI a namespace URI
         * @return a collection of wildcard appearance instances that cover
         *         the specified namespace
         */
        public Collection<ParticleAppearance> getCandidateParticles(
                String nsURI);
    }
}
