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
 * @(#)EncoderProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

import java.util.Map;
import java.util.Set;

import javax.xml.transform.URIResolver;

/**
 * Each encoder provider which does not use the OtdRoot/DOM conversion
 * mechanism, which is meant for legacy encoders ported from OTDs, needs
 * to implement this interface and register the implementation class in
 * encoder type definition file.  The implementation class of this interface
 * must provide a default constructor, which does not take any parameters.
 *  
 * @author Jun Xu
 */
public interface EncoderProvider {
    
    /**
     * Gets the identification of the encoder provider.  This identification
     * currently is in the form such as "customencoder-1.0", "hl7encoder-1.0".
     * A provider is recommended to following this naming convention and may
     * add their organization specific prefixes.  Theoretically providers can
     * use any identification string they want as long as it is unique in the
     * environment.  This value will be stored in the encoder type created
     * from this provider.
     * 
     * @return the identification of the encoder provider
     */
    public String getIdentification();
    
    /**
     * Gets the aliases of the encoder type.
     * 
     * @return the aliases of the encoder type or an empty string there is
     *          no alias.
     */
    public String[] getAliases();
    
    /**
     * Instantiates a provider specific instance of <code>Encoder</code> based
     * on a path to an XSD and the qualified name of a global element (both
     * retrieved from the MetaRef instance), and an instance of URIResolver
     * if applicable.  If URIResolver is <code>null</code>, then file system
     * is assumed, and the path returned from a <code>MetaRef</code> instance
     * is assumed to be a file location.
     * 
     * @param metaRef metadata reference pointing to an XSD
     * @param uriResolver URI resolver used to resolve a path to an XML source 
     * @return an instance of <code>Encoder</code> that is bound the specified
     *         metadata
     * @throws EncoderConfigurationException thrown if anything wrong during
     *           the instantiation of the encoder instance
     */
    public Encoder newEncoder(MetaRef metaRef, URIResolver uriResolver)
            throws EncoderConfigurationException;
    
    /**
     * Instantiates a collection of provider specific instances of
     * <code>Encoder</code> based on a collection of metadata references,
     * and an instance of URIResolver if applicable.  If URIResolver is
     * <code>null</code>, then file system is assumed, and the path returned
     * from a MetaRef instance is assume to be a file location.
     * 
     * @param metaRefs a collection of metadata references (instances of
     *           <code>MetaRef</code>)
     * @param uriResolver URI resolver used to resolve a path to an XML source 
     * @return a map of encoder instances, in which the keys are instances of
     *          <code>MetaRef</code> and the values are instances of
     *          <code>Encoder</code>
     * @throws EncoderConfigurationException thrown if anything wrong during
     *           the instantiation of the encoder instances
     * @see EncoderProvider#newEncoder(MetaRef, URIResolver)
     */
    public Map newEncoders(Set metaRefs, URIResolver uriResolver)
            throws EncoderConfigurationException;
    
    /**
     * Instantiates a provider specific instance of <code>Encoder</code> based
     * on a path to an XSD and the qualified name of a global element (both
     * retrieved from the MetaRef instance), and an instance of URIResolver
     * if applicable.  If URIResolver is <code>null</code>, then file system
     * is assumed, and the path returned from a <code>MetaRef</code> instance
     * is assumed to be a file location. This method supports passing in
     * additional encoder properties.
     * 
     * @param metaRef metadata reference pointing to an XSD
     * @param uriResolver URI resolver used to resolve a path to an XML source
     * @param properties the additional encoder properties
     * @return an instance of <code>Encoder</code> that is bound the specified
     *         metadata
     * @throws EncoderConfigurationException thrown if anything wrong during
     *           the instantiation of the encoder instance
     */
    public Encoder newEncoder(MetaRef metaRef, URIResolver uriResolver,
            EncoderProperties properties)
            throws EncoderConfigurationException;
    
    /**
     * Instantiates a collection of provider specific instances of
     * <code>Encoder</code> based on a collection of metadata references,
     * and an instance of URIResolver if applicable.  If URIResolver is
     * <code>null</code>, then file system is assumed, and the path returned
     * from a MetaRef instance is assume to be a file location.
     * 
     * @param metaRefs a collection of metadata references (instances of
     *           <code>MetaRef</code>)
     * @param uriResolver URI resolver used to resolve a path to an XML source
     * @param properties the additional encoder properties 
     * @return a map of encoder instances, in which the keys are instances of
     *          <code>MetaRef</code> and the values are instances of
     *          <code>Encoder</code>
     * @throws EncoderConfigurationException thrown if anything wrong during
     *           the instantiation of the encoder instances
     * @see EncoderProvider#newEncoder(MetaRef, URIResolver)
     */
    public Map newEncoders(Set metaRefs, URIResolver uriResolver,
            EncoderProperties properties)
            throws EncoderConfigurationException;
    
    /**
     * Queries the data nature of the encoder provider.  This value will be
     * stored in the encoder type created from this provider.
     * 
     * @return the data nature of the encoder provider
     */
    public DataNature getDataNature();
    
    /**
     * Sets the related encoder type of this provider, so the encoder type
     * can be passed to the encoders created from this provider.  Encoder type
     * is created by encoder framework, one for each provider.  Encoder type is
     * the facility that provides read only information that encoder provider
     * supplies to encoder users (consumers) but blocks users from directly
     * interacting with the provider.
     * 
     * @param encoderType the encoder provider
     */
    public void setType(EncoderType encoderType);
}
