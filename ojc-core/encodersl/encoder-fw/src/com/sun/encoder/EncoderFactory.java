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
 * @(#)EncoderFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;

import javax.xml.namespace.QName;

import javax.xml.transform.URIResolver;

/**
 * The instance of this class (an encoder factory) manages all encoder types
 * supported in the encoder framework. In order to be discovered by the
 * encoder factory, a pluggable encoder type should implement a service
 * provider interface (SPI) <code>com.sun.encoder.EncoderProvider</code>.
 *
 * @author Jun Yang, Jun Xu
 * @version
 */
public class EncoderFactory {

    private static final String PROVIDER_PREFIX = "META-INF/services/";

    /*
     * The singleton encoder factory instance
     */
    private static final EncoderFactory mInstance = new EncoderFactory();

    /*
     * Encoder type set used by allEncoderTypes() method
     */
    private static final Set<EncoderType> mEncoderTypeSet =
        new HashSet<EncoderType>();

    /*
     * Encoder type map
     */
    private static final Map<String, EncoderType> mEncoderTypeMap =
        new HashMap<String, EncoderType>();

    /*
     * Encoder provider map
     */
    private static final Map<String, EncoderProvider> mEncoderProviderMap =
        new HashMap<String, EncoderProvider>();

    /*
     * Discovers and loads all encoder providers and populates encoder
     * type collections.
     */
    static {
        loadAllEncoderProviders(mEncoderProviderMap);
        populateEncoderTypes(mEncoderTypeMap, mEncoderTypeSet,
                mEncoderProviderMap);
    }

    /**
     * Discovers service providers that follows the META-INF/services pattern.
     *
     * @param clazz the abstract class that the provider implements
     * @param loader the class loader used for service lookup
     * @return a collection of provider instances that implement the
     *         specified abstract class
     */
    private static <T> Iterator<T> getProviders(
            Class<T> clazz, ClassLoader loader) {
        BufferedReader in = null;
        try {
            Enumeration providers =
                loader.getResources(PROVIDER_PREFIX + clazz.getName());
            List<T> list = new ArrayList<T>();
            while (providers.hasMoreElements()) {
                URL url = (URL) providers.nextElement();
                in = new BufferedReader(
                        new InputStreamReader(url.openStream(), "UTF-8"));
                String s;
                while ((s = in.readLine()) != null) {
                    s = s.trim();
                    if (s.length() == 0 || s.charAt(0) == '#') {
                        continue;
                    }
                    list.add(
                            Class.forName(s, true, loader).asSubclass(
                                    clazz).newInstance());
                }
            }
            return list.iterator();
        } catch (IOException e) {
            throw new RuntimeException(
                    "Unable to load providers for '"
                    + clazz.getName() + "'", e);
        } catch (InstantiationException e) {
            throw new RuntimeException(
                    "Unable to load providers for '"
                    + clazz.getName() + "'", e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(
                    "Unable to load providers for '"
                    + clazz.getName() + "'", e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(
                    "Unable to load providers for '"
                    + clazz.getName() + "'", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException ex) {
                    //ignore
                }
            }
        }
    }

    /**
     * discovers all encoder service providers.
     *
     * @return a map of encoder providers with key on encoder provider
     *         identification
     */
    private static void loadAllEncoderProviders(
            Map<String, EncoderProvider> providerMap) {
        Iterator<ClassLoaderManager> iter =
            getProviders(ClassLoaderManager.class,
                EncoderFactory.class.getClassLoader());
        Set<ClassLoader> loaderSet = new HashSet<ClassLoader>();
        ClassLoader loader;
        while (iter.hasNext()) {
            ClassLoaderManager loaderMgr = iter.next();
            loader = loaderMgr.getEncoderClassLoader();
            if (!loaderSet.contains(loader)) {
                loadEncoderProviders(providerMap, loader);
                loaderSet.add(loader);
            }
        }
        loader = EncoderFactory.class.getClassLoader();
        if (loader != null && !loaderSet.contains(loader)) {
            loadEncoderProviders(providerMap, loader);
            loaderSet.add(loader);
        }
        loader = Thread.currentThread().getContextClassLoader();
        if (loader != null && !loaderSet.contains(loader)) {
            loadEncoderProviders(providerMap, loader);
            loaderSet.add(loader);
        }
    }

    /**
     * Discovers encoder providers within one class loader.
     *
     * @param providerMap the encoder provider map to be populated
     * @param loader the class loader
     */
    private static void loadEncoderProviders(
            Map<String, EncoderProvider> providerMap, ClassLoader loader) {
        Iterator<EncoderProvider> iter =
            getProviders(EncoderProvider.class, loader);
        while (iter.hasNext()) {
            EncoderProvider provider = iter.next();
            if (!providerMap.containsKey(provider.getIdentification())) {
                providerMap.put(provider.getIdentification(), provider);
            }
            String[] aliases = provider.getAliases();
            if (aliases == null || aliases.length == 0) {
                continue;
            }
            for (int i = 0; i < aliases.length; i++) {
                if (!providerMap.containsKey(aliases[i])) {
                    providerMap.put(aliases[i], provider);
                }
            }
        }
    }

    /**
     * Populates encoder type map and set from the encoder provider map.
     *
     * @param typeMap encoder type map to be populated
     * @param typeSet encoder type set to be populated
     * @param providerMap encoder provider map to be read from
     */
    private static void populateEncoderTypes(
            Map<String, EncoderType> typeMap,
            Set<EncoderType> typeSet,
            Map<String, EncoderProvider> providerMap) {
        Map<EncoderProvider, EncoderType> provider2TypeMap =
            new HashMap<EncoderProvider, EncoderType>();
        Iterator<Map.Entry<String, EncoderProvider>> iter =
            providerMap.entrySet().iterator();
        EncoderType ecType;
        for (; iter.hasNext();) {
            Map.Entry<String, EncoderProvider> entry = iter.next();
            if (!provider2TypeMap.containsKey(entry.getValue())) {
                ecType =
                    new EncoderType(entry.getKey(),
                            entry.getValue().getDataNature());
                entry.getValue().setType(ecType);
                typeSet.add(ecType);
                provider2TypeMap.put(entry.getValue(), ecType);
            } else {
                ecType = provider2TypeMap.get(entry.getValue());
            }
            typeMap.put(entry.getKey(), ecType);
        }
    }

    /**
     * Private constructor that forces the instance to be created from the
     * method <code>newInstance()</code>.
     */
    private EncoderFactory () {
        //no-op
    }

    /**
     * Creates a new factory instance.
     */
    public static EncoderFactory newInstance()
        throws EncoderConfigurationException {
        return mInstance;
    }

    /**
     * Instantiates an encoder instance. This process might be expensive,
     * Please cache the encoder instance returned from this method if further
     * use of it is needed.
     *
     * @param type the identification of the encoder type
     * @param xsd the metadata reference to the XSD
     * @return an instance of <code>Encoder</code>
     * @throws EncoderConfigurationException thrown if anything goes wrong
     *          during instantiation of the encoder instance
     */
    public Encoder newEncoder(EncoderType type, MetaRef xsd)
            throws EncoderConfigurationException {
        if (type == null) {
            throw new EncoderConfigurationException("no EncoderType");
        }
        if (xsd == null)  {
            throw new EncoderConfigurationException("no xsd");
        }

        URIResolver uriResolver = null;
        if (type.isBasic()) {
            return mEncoderProviderMap.get(
                    type.getIdentification()).newEncoder(xsd, uriResolver);
        } else {
            return mEncoderProviderMap.get(
                    type.getIdentification()).newEncoder(xsd, uriResolver,
                    type.getEncoderProperties());
        }
    }

    /**
     * Creates encoders in batch mode.  This will give encoder API a chance
     * to optimize the creation process.  Especially when the set of XSDs that
     * back-up the encoders are related (such as several base XSDs referenced
     * by a bunch of top level XSDs), this method offers an opportunity to
     * creating only one XML type system that is shared by all encoders created
     * from this batch.
     *
     * <p>This process might be expensive. Please cache the encoder instances
     * returned from this method if further use of them is needed.
     *
     * @param type the encoder type
     * @param metaRefSet a set of instances of MetaRef
     * @return a map of encoders, whereas the keys are instances of MetaRef and
     *         the values are instances of Encoder
     * @throws EncoderConfigurationException thrown when anything wrong
     *               happened during creation of encoders
     */
    public Map newEncoders(EncoderType type, Set metaRefSet)
            throws EncoderConfigurationException {
        if (type == null) {
            throw new EncoderConfigurationException("no EncoderType.");
        }
        if (metaRefSet == null)  {
            throw new EncoderConfigurationException("no MetaRef set.");
        }

        if (type.isBasic()) {
            return mEncoderProviderMap.get(
                    type.getIdentification()).newEncoders(metaRefSet, null);
        }
        return mEncoderProviderMap.get(
                type.getIdentification()).newEncoders(metaRefSet, null,
                        type.getEncoderProperties());
    }

    /**
     * This method is deprecated.  New code please use the makeMeta method
     * with two parameters: path and root element QName.  For using user
     * defined encoder (UD1 encoder), calling this method may be still OK, but
     * to use HL7 encoder, calling this method will not work.
     *
     * @deprecated
     * @param path the path to an XSD file
     * @return an instance of MetaRef which represents the metadata of an
     *         encoder
     */
    public MetaRef makeMeta(String path) {
        return new SimpleMetaRef(path);
    }

    /**
     * Creates an instance of MetaRef, which represents the metadata of an
     * encoder.  To use new encoders which are not populated from 5.1.X,
     * it is strongly recommended to call the method with root element name
     * instead of calling the old <code>makeMeta</code> method.
     *
     * @param path the file path to the XSD file
     * @param rootElemName root element's QName
     * @return an instance of MetaRef, which represents the metadata of an
     *          encoder
     */
    public MetaRef makeMeta(String path, QName rootElemName) {
        return new SimpleMetaRef(path, rootElemName);
    }

    /**
     * Creates an instance of MetaRef, which represents the metadata of an
     * encoder.  To use new encoders which are not populated from 5.1.X,
     * it is strongly recommended to call the method with root element name
     * instead of calling the old <code>makeMeta</code> method.
     *
     * @param url the URL of the XSD file
     * @param rootElemName root element's QName
     * @return an instance of MetaRef, which represents the metadata of an
     *          encoder
     */
    public MetaRef makeMeta(URL url, QName rootElemName) {
        return new SimpleMetaRef(url, rootElemName);
    }

    /**
     * Returns all Encoder types supported in the Encoder framework
     */
    public Set allEncoderTypes() {
        return mEncoderTypeSet;
    }

    /**
     * Returns an encoder type from the given identification. Most of the
     * encoder type information except the identification should not be
     * visible to the modules outside of the encoder framework. The returned
     * encoder type is further used to look up encoder instances.
     *
     * @param encodingStyle the encoding style in the form of
     *       <code>identification?query_string</code>.  For example,
     *       the encoding style <code>
     *       "cocoencoder-1.0?displayCharset=EBCDIC&display1Charset=Cp834"
     *       </code> indicates an encoder identification <code>cocoencoder-1.0
     *       </code> and two custom properties <code>displayCharset</code>
     *       and <code>display1Charset</code>
     * @return an instance of <code>EncoderType</code>
     * @throws EncoderConfigurationException
     */
    public EncoderType makeType(String encodingStyle)
        throws EncoderConfigurationException {
        String idOrAlias = extractIdOrAlias(encodingStyle);
        if (!mEncoderTypeMap.containsKey(idOrAlias)) {
            throw new EncoderConfigurationException(
                    "No Encoder : <" + idOrAlias
                    + "> in the installed Encoder SharedLibrary. Please install"
                    + " a new Encoder SharedLibrary that contains it");
        }
        EncoderProperties props;
        try {
            props = extractProperties(encodingStyle);
        } catch (UnsupportedEncodingException e) {
            throw new EncoderConfigurationException(e);
        }
        if (props == null || props.isEmpty()) {
            return mEncoderTypeMap.get(idOrAlias);
        }
        return new EncoderType(mEncoderTypeMap.get(idOrAlias), props);
    }

    /**
     * Instantiates an encoder instance. This process might be expensive,
     * Please cache the encoder instance returned from this method if further
     * use of it is needed. This method supports passing in additional
     * properties.
     *
     * @param type the identification of the encoder type
     * @param xsd the metadata reference to the XSD
     * @param properties EncoderProperties object
     * @return an instance of <code>Encoder</code>
     * @throws EncoderConfigurationException thrown if anything goes wrong
     *          during instantiation of the encoder instance
     */
    public Encoder newEncoder(EncoderType type, MetaRef xsd,
            EncoderProperties properties)
            throws EncoderConfigurationException {
        if (type == null) {
            throw new EncoderConfigurationException("no EncoderType");
        }
        if (xsd == null)  {
            throw new EncoderConfigurationException("no xsd");
        }

        EncoderProperties props;
        if (!type.isBasic()) {
            props = type.getEncoderProperties().cloneMutable();
            props.addAll(properties);
        } else {
            props = properties;
        }
        return mEncoderProviderMap.get(
                type.getIdentification()).newEncoder(xsd, null, props);
    }

    /**
     * Creates encoders in batch mode.  This will give encoder API a chance
     * to optimize the creation process.  Especially when the set of XSDs that
     * back-up the encoders are related (such as several base XSDs referenced
     * by a bunch of top level XSDs), this method offers an opportunity to
     * creating only one XML type system that is shared by all encoders created
     * from this batch. This method supports passing in additional encoder
     * properties.
     *
     * <p>This process might be expensive. Please cache the encoder instances
     * returned from this method if further use of them is needed.
     *
     * @param type the encoder type
     * @param metaRefSet a set of instances of MetaRef
     * @param properties the additional encoder properties
     * @return a map of encoders, whereas the keys are instances of MetaRef and
     *         the values are instances of Encoder
     * @throws EncoderConfigurationException thrown when anything wrong
     *               happened during creation of encoders
     */
    public Map newEncoders(EncoderType type, Set metaRefSet,
            EncoderProperties properties)
            throws EncoderConfigurationException {
        if (type == null) {
            throw new EncoderConfigurationException("no EncoderType.");
        }
        if (metaRefSet == null)  {
            throw new EncoderConfigurationException("no MetaRef set.");
        }

        EncoderProperties props;
        if (!type.isBasic()) {
            props = type.getEncoderProperties().cloneMutable();
            props.addAll(properties);
        } else {
            props = properties;
        }
        return mEncoderProviderMap.get(
                type.getIdentification()).newEncoders(metaRefSet, null, props);
    }

    /**
     * Extracts the encoder identification or alias from an encoding style.
     *
     * @param encodingStyle the encoding style
     * @return the encoder identification
     */
    private String extractIdOrAlias(String encodingStyle) {
        int pos = encodingStyle.indexOf('?');
        if (pos == -1) {
            return encodingStyle;
        }
        return encodingStyle.substring(0, pos);
    }

    /**
     * Extracts encoder properties from an encoding style.
     *
     * @param encodingStyle the encoding style
     * @return the encoder properties. May returns <code>null</code> if
     *         there are no properties.
     * @throws UnsupportedEncodingException
     */
    private EncoderProperties extractProperties(String encodingStyle)
            throws UnsupportedEncodingException {
        int pos = encodingStyle.indexOf('?');
        if (pos == -1) {
            return null;
        }
        encodingStyle = encodingStyle.substring(pos + 1);
        String[] propStrs = encodingStyle.split("\\&");
        if (propStrs == null || propStrs.length == 0) {
            return null;
        }
        String[] keyValueStr;
        EncoderProperties properties = new EncoderProperties();
        for (int i = 0; i < propStrs.length; i++) {
            keyValueStr = propStrs[i].split("=");
            if (keyValueStr == null || keyValueStr.length == 0) {
                continue;
            }
            if (keyValueStr.length == 1) {
                properties.setProperty(
                        URLDecoder.decode(keyValueStr[0], "UTF-8"));
            } else {
                properties.setProperty(
                        URLDecoder.decode(keyValueStr[0], "UTF-8"),
                        URLDecoder.decode(keyValueStr[1], "UTF-8"));
            }
        }
        return properties;
    }

    /**
     * A simple implementation of the MetaRef interface.
     */
    private class SimpleMetaRef implements MetaRef {

        private final String mPath;
        private final URL mURL;
        private final QName mRootElemName;
        private final String mToString;

        /**
         * Constructs a MetaRef object given main meta file path
         */
        protected SimpleMetaRef(String mainFile) {
            this(mainFile, null);
        }

        /**
         * Constructs a MetaRef object given main meta file path and
         * root element QName
         */
        protected SimpleMetaRef(String mainFile, QName rootElemName) {
            mPath = mainFile;
            mURL = null;
            mRootElemName = rootElemName;
            StringBuffer sb = new StringBuffer();
            sb.append('[').append(mPath).append(']');
            if (mRootElemName != null) {
                sb.append(mRootElemName);
            }
            mToString = sb.toString();
        }

        /**
         * Constructs a MetaRef object given main meta file path and
         * root element QName
         */
        protected SimpleMetaRef(URL mainFile, QName rootElemName) {
            mPath = null;
            mURL = mainFile;
            mRootElemName = rootElemName;
            StringBuffer sb = new StringBuffer();
            sb.append('[').append(mURL.toString()).append(']');
            if (mRootElemName != null) {
                sb.append(mRootElemName);
            }
            mToString = sb.toString();
        }

        /**
         * Returns the path of the main metadata file. The path should point to
         * an XSD file on the local file system. {@link MetaRef#getPath()}
         *
         * @return the path of the main meta file
         */
        public String getPath() {
            return mPath;
        }

        /**
         * Returns the URL of the main metadata file. {@link MetaRef#getURL()}
         */
        public URL getURL() {
            return mURL;
        }

        /**
         * Return the QName of the root element.
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }

        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof SimpleMetaRef)) {
                return false;
            }
            return mToString.equals(((SimpleMetaRef) obj).mToString);
        }

        @Override
        public int hashCode() {
            return mToString.hashCode();
        }

        @Override
        public String toString() {
            return mToString;
        }
    }
}
