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
 * @(#)NodesFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.custom.runtime.provider;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaAnnotation;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;

import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.runtime.CoderFactory;
import com.sun.encoder.custom.appinfo.CustomEncoding;
import com.sun.encoder.custom.appinfo.Delimiter;
import com.sun.encoder.custom.appinfo.DelimiterLevel;
import com.sun.encoder.custom.appinfo.NodeProperties;
import com.sun.encoder.custom.appinfo.Delimiter.Kind;
import com.sun.encoder.custom.appinfo.Delimiter.OptionalMode;
import com.sun.encoder.custom.appinfo.Delimiter.TerminatorMode;
import com.sun.encoder.custom.appinfo.NodeProperties.Alignment;
import com.sun.encoder.custom.appinfo.NodeProperties.NodeType;
import com.sun.encoder.custom.appinfo.NodeProperties.Order;
import com.sun.encoder.custom.appinfo.NodeProperties.UndefDataPolicy;
import com.sun.encoder.custom.runtime.Delim.Mode;
import com.sun.encoder.custom.runtime.Delim.Type;
import com.sun.encoder.custom.runtime.provider.Nodes.Node;
import com.sun.encoder.custom.runtime.provider.Nodes.DelimForFixedLen;
import com.sun.encoder.runtime.provider.Misc;

/**
 * This is a factory class that creates and populates an instance of Nodes
 * (descriptor for a specific custom encoder) from various metadata sources.
 * Currently it supports loading the descriptors from an XSD file with
 * &lt;appinfo&gt; extension.
 *
 * @author Jun Xu
 * @since 6.0
 */
public class NodesFactory {

    /**
     * The element qualified name for the element that is used in AppInfo to
     * describe the custom properties for custom encoders
     */
    public static final QName NODE_PROP_ELEMENT =
        new QName("urn:com.sun:encoder-custom-1.0", "nodeProperties");

    /**
     * The qualified name of the "source" attribute in the "appinfo" element
     */
    public static final QName APPINFO_SOURCE_ATTR = new QName("source");

    /**
     * The namespace URN that represents the Sun encoder extension to XSD
     */
    public static final String ENCODER_NAMESPACE = "urn:com.sun:encoder";

    /**
     * Creates and populates an instance of Nodes from an XSD file with
     * custom encoder &lt;appinfo&gt; extension.
     *
     * @param xsdPath the XSD file path
     * @param rootElementName the qualified name of the root element
     * @return an instance of Nodes
     * @throws EncoderConfigurationException thrown when any invalidity found
     *                  in the XSD
     */
    public static Nodes loadFromXSD(File xsdPath, QName rootElementName)
            throws EncoderConfigurationException {

        SchemaDocument schemaXmlObj;
        try {
            SchemaTypeLoader typeLoader =
                XmlBeans.typeLoaderForClassLoader(
                        NodeProperties.class.getClassLoader());
            schemaXmlObj = SchemaDocument.Factory.parse(xsdPath);
            SchemaTypeSystem schemaTS =
                XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                        typeLoader, null);
            typeLoader =
                XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{typeLoader, schemaTS});
            SchemaGlobalElement rootElement
                = typeLoader.findElement(rootElementName);
            if (rootElement == null) {
                throw new EncoderConfigurationException(
                        "Unable to find element " + rootElementName
                        + " in " + xsdPath);
            }
            Nodes nodes = loadFromXSD(rootElement);
            nodes.setMetadataLocation(xsdPath.toURI().toURL());
            nodes.setMetadataNamespace(schemaXmlObj.getSchema().getTargetNamespace());
            return nodes;
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e);
        } catch (IOException e) {
            throw new EncoderConfigurationException(e);
        }
    }

    /**
     * Creates and populates an instance of Nodes from an XSD file (located
     * via a URL) with custom encoder &lt;appinfo&gt; extension.
     *
     * @param xsdURL the XSD file URL
     * @param rootElementName the qualified name of the root element
     * @return an instance of Nodes
     * @throws EncoderConfigurationException thrown when any invalidity found
     *                  in the XSD
     */
    public static Nodes loadFromXSD(URL xsdURL, QName rootElementName)
            throws EncoderConfigurationException {

        SchemaDocument schemaXmlObj;
        try {
            SchemaTypeLoader typeLoader =
                XmlBeans.typeLoaderForClassLoader(
                        NodeProperties.class.getClassLoader());
            XmlOptions options = new XmlOptions();
            options.put(XmlOptions.COMPILE_DOWNLOAD_URLS, Boolean.TRUE);
            schemaXmlObj = SchemaDocument.Factory.parse(xsdURL, options);
            SchemaTypeSystem schemaTS =
                XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                        typeLoader, options);
            typeLoader =
                XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{typeLoader, schemaTS});
            SchemaGlobalElement rootElement
                = typeLoader.findElement(rootElementName);
            if (rootElement == null) {
                throw new EncoderConfigurationException(
                        "Unable to find element " + rootElementName
                        + " in " + xsdURL);
            }
            Nodes nodes = loadFromXSD(rootElement);
            nodes.setMetadataLocation(xsdURL);
            nodes.setMetadataNamespace(schemaXmlObj.getSchema().getTargetNamespace());
            return nodes;
        } catch (XmlException e) {
            throw new EncoderConfigurationException(e);
        } catch (IOException e) {
            throw new EncoderConfigurationException(e);
        }
    }

    /**
     * Creates and populates an instance of Nodes from an XSD global
     * element declaration with custom encoder &lt;appinfo&gt; extension.
     *
     * @param rootElement the global element declaration
     * @return an instance of Nodes
     * @throws EncoderConfigurationException thrown when any invalidity found
     *                  in the XSD
     */
    public static Nodes loadFromXSD(SchemaGlobalElement rootElement)
            throws EncoderConfigurationException {

        /* the map mapping from node key to node index (to be populated) */
        Map<String, Integer> nodeMap = new HashMap<String, Integer>();
        /* list of RawNode object. */
        List<RawNode> rawNodeList = new ArrayList<RawNode>();
        /* the delimiter list. */
        List<Delim> delimList = new ArrayList<Delim>();
        // Reserve the first one for the global escape sequence if any.
        // Use some kind of dummy data just to bypass the checking.
        Delim globalEscDelim = new Delim(Delim.Type.ESCAPE_NEXT,
                (byte) 100, new byte[]{(byte) 0});
        delimList.add(globalEscDelim);
        /* the slot (embedded delimiter) list. */
        List<Delim.Slot> slotList = new ArrayList<Delim.Slot>();

        Map<SchemaParticle, List<Integer>> contentModelMap = new HashMap<SchemaParticle, List<Integer>>();

        // Load and populate rawNodeList, nodeMap, delimList and slotList
        // starting from root. It returns index for the top/root node.
        int rootIndex = -1;
        {
            String parentKey = null;
            int indexWithinParent = -1;
            rootIndex = loadRawNode(parentKey, indexWithinParent, rootElement,
                    nodeMap, rawNodeList, delimList, slotList, contentModelMap);
        }
        // converts populated delim list to an array
        Delim[] delims = new Delim[delimList.size()];
        delimList.toArray(delims);
        // converts populated embedded delim list to an array
        Delim.Slot[] slots = new Delim.Slot[slotList.size()];
        slotList.toArray(slots);
        // get the root rawNode.
        RawNode rootRawNode = rawNodeList.get(rootIndex);
        // check to see if any global escape sequence. if not, clean first one.
        if (rootRawNode.mEscapeSequence == null) {
            delims[0] = null;
        } else {
            delims[0].mBytes = rootRawNode.mEscapeSequence;
        }

        // Now converts RawNode objects into Nodes object which contains
        // a list of Node objects.
        OtdDelim otdDelim = new OtdDelim(delims, slots);
        Nodes nodes = new Nodes(rawNodeList.size(), otdDelim);

        // handles antecoding and decoding
        if (rootRawNode.mAntecoding != null
                || rootRawNode.mDecoding != null
                || rootRawNode.mPreDecodeCharCoding != null) {
            // antecoding priority: Antecoding > DEFAULT_ANTECODING
            String antecoding = rootRawNode.mAntecoding;
            if (antecoding == null) {
                antecoding = Nodes.DEFAULT_ANTECODING;
            }
            // decoding priority: PreDecodeCharCoding > Decoding > DEFAULT_DECODING
            String decoding = rootRawNode.mPreDecodeCharCoding;
            if (decoding == null) {
                decoding = rootRawNode.mDecoding;
            }
            if (decoding == null) {
                decoding = Nodes.DEFAULT_DECODING;
            }
            nodes.setAntecoder(CoderFactory.getTrans(antecoding, decoding));
        }
        if (rootRawNode.mDecoding != null) {
            nodes.setDecoder(CoderFactory.getCoder(rootRawNode.mDecoding));
        }
        if (rootRawNode.mAntecoding != null) {
            nodes.setAnteCoding(rootRawNode.mAntecoding);
        }

        // handles encoding and postcoding
        if (rootRawNode.mEncoding != null) {
            nodes.setEncoder(CoderFactory.getCoder(rootRawNode.mEncoding));
        }
        if (rootRawNode.mPostcoding != null
                || rootRawNode.mEncoding != null
                || rootRawNode.mPostEncodeCharCoding != null) {
            // postcoding priority: Postcoding > DEFAULT_POSTCODING
            String postcoding = rootRawNode.mPostcoding;
            if (postcoding == null) {
                postcoding = Nodes.DEFAULT_POSTCODING;
            }
            // encoding priority: PostDecodeCharCoding > Encoding > DEFAULT_ENCODING
            String encoding = rootRawNode.mPostEncodeCharCoding;
            if (encoding == null) {
                encoding = rootRawNode.mEncoding;
            }
            if (encoding == null) {
                encoding = Nodes.DEFAULT_ENCODING;
            }
            nodes.setPostcoder(CoderFactory.getTrans(encoding, postcoding));
        }
        if (rootRawNode.mPostcoding != null) {
            nodes.setPostCoding(rootRawNode.mPostcoding);
        }

        nodes.setUndefDataPolicy(rootRawNode.mUndefDataPolicy);
        nodes.setFineGrainedInherit(rootRawNode.mFineGrainedInherit);
        // now converting RawNode into Nodes.Node
        final int nodeCount = rawNodeList.size();
        for (int i = 0; i < nodeCount; i++) {
            RawNode n = rawNodeList.get(i);
            int maxOccurs = n.mMaxOccurs;
            if (i == rootIndex) {
                // the repeat for top node should always be false
                maxOccurs = 1;
            }
            try {
                nodes.addNode(i, n.mFromId, n.mNodeId, n.mType, n.mNamespace,
                        n.mName, n.mChildIdx, n.mOrder, n.mLength, n.mAlign,
                        n.mMatch, n.mSubnodes, n.mLevel, n.mOffset, n.mPosition,
                        n.mDefaultValue, n.mScavengerChars, n.mOutput1stChar,
                        n.mMinNOfChildren, n.mMaxNOfChildren, n.mMinOccurs,
                        maxOccurs, n.mNoMatch, n.mDelimForFixedLen, n.mElmType);
            } catch (IllegalArgumentException e) {
                throw new EncoderConfigurationException("Node={" + n.mNamespace
                        + "}" + n.mName + ": " + e.getMessage());
            }
        }
        nodes.setRootNodeIndex(rootIndex);
        if (nodes.isFineGrainedInherit()) {
            nodes.buildFineGrainedInherit();
        }
        return nodes;
    }

    /**
     * Loads custom properties from the &lt;appinfo&gt; of an element
     * declaration.
     *
     * @param elem the element declaration
     * @return the custom encoding object
     */
    private static CustomEncoding getCustomEncoding(SchemaLocalElement elem)
            throws XmlException {
        SchemaAnnotation anno = elem.getAnnotation();
        if (anno == null) {
            return null;
        }
        XmlObject[] xmlObjs = anno.getApplicationInformation();
        if (xmlObjs == null || xmlObjs.length == 0) {
            return null;
        }
        for (int i = 0; i < xmlObjs.length; i++) {
            XmlCursor cursor = xmlObjs[i].newCursor();
            String source = cursor.getAttributeText(APPINFO_SOURCE_ATTR);
            cursor.dispose();
            if (!ENCODER_NAMESPACE.equals(source)) {
                continue;
            }
            if (xmlObjs[i] instanceof CustomEncoding) {
                return (CustomEncoding) xmlObjs[i];
            } else {
                return CustomEncoding.Factory.parse(
                        xmlObjs[i].xmlText());
            }
        }
        return null;
    }

    /**
     * Gets the node type value as defined by <code>Nodes.Node.Type</code>
     * from the custom properties.
     *
     * @param nodeProps the custom properties
     * @return the node type as one of the enumerations defined in
     *              <code>Nodes.Node.Type</code>
     */
    private static Node.Type getNodeType(NodeProperties nodeProps) {
        switch (nodeProps.getNodeType().intValue()) {
            case NodeType.INT_ARRAY:
                return Node.Type.ARRAY;
            case NodeType.INT_DELIMITED:
                return Node.Type.DELIM;
            case NodeType.INT_FIXED_LENGTH:
                return Node.Type.FIXED;
            case NodeType.INT_GROUP:
                return Node.Type.GROUP;
            case NodeType.INT_TRANSIENT:
                return Node.Type.TRANS;
            default:
                throw new IllegalArgumentException(
                    "Invalid node type: " + nodeProps.getNodeType());
        }
    }

    /**
     * Gets the order value as defined by <code>Nodes.Node.Order</code>
     * from the custom properties.
     *
     * @param nodeProps the custom properties
     * @return the order as one of the enumerations defined in
     *              <code>Nodes.Node.Order</code>
     */
    private static Node.Order getOrder(NodeProperties nodeProps) {
        if (!nodeProps.isSetOrder()) {
            return Node.Order.SEQ;
        }
        switch (nodeProps.getOrder().intValue()) {
            case Order.INT_SEQUENCE:
                return Node.Order.SEQ;
            case Order.INT_ANY:
                return Node.Order.ANY;
            case Order.INT_MIXED:
                return Node.Order.MIX;
            default:
                throw new IllegalArgumentException(
                    "Invalid order: " + nodeProps.getOrder());
        }
    }

    /**
     * Gets the alignment value as defined by <code>Nodes.Node.Align</code>
     * from the custom properties.
     *
     * @param nodeProps the custom properties
     * @return the alignment as one of the enumerations defined in
     *              <code>Nodes.Node.Align</code>
     */
    private static Node.Align getAlign(NodeProperties nodeProps) {
        if (!nodeProps.isSetAlignment()) {
            return Node.Align.EXACT;
        }
        switch (nodeProps.getAlignment().intValue()) {
            case Alignment.INT_BEGIN:
                return Node.Align.BEGIN;
            case Alignment.INT_BLIND:
                return Node.Align.BLIND;
            case Alignment.INT_EXACT:
                return Node.Align.EXACT;
            case Alignment.INT_FINAL:
                return Node.Align.FINAL;
            case Alignment.INT_INTER:
                return Node.Align.INTER;
            case Alignment.INT_ONEOF:
                return Node.Align.ONEOF;
            case Alignment.INT_SUPER:
                return Node.Align.SUPER;
            case Alignment.INT_REGEX:
                return Node.Align.REGEX;
            default:
                throw new IllegalArgumentException(
                    "Invalid alignment: " + nodeProps.getAlignment());
        }
    }

    /**
     * Gets the matching bytes from the custom properties.
     *
     * @param nodeProps the custom properties
     * @return the matching bytes
     */
    private static byte[] getMatch(NodeProperties nodeProps) {
        if (!nodeProps.isSetMatch()) {
            return null;
        }
        return Misc.str2bytes(Misc.nonPrintable(nodeProps.getMatch()));
    }

    /**
     * Loads an instance of <code>Delim</code> from the raw delimiter
     * description element.
     *
     * @param rawDelim the element that describes delimiter levels
     * @param slotList embedded delimiter slot list
     * @return an instance of <code>Delim</code>, which can be accepted by
     *          <code>OtdDelim</code>
     * @throws EncoderConfigurationException
     */
    private static Delim loadDelimiter(Delimiter rawDelim,
            List<Delim.Slot> slotList) throws EncoderConfigurationException {

        Type type;
        byte prec = 10;
        Mode term;
        Mode lieu;
        if (!rawDelim.isSetKind()) {
            type = Type.NORMAL;
        } else {
            switch (rawDelim.getKind().intValue()) {
                case Kind.INT_NORMAL:
                    type = Type.NORMAL;
                    break;
                case Kind.INT_REPEAT:
                    type = Type.REPEAT;
                    break;
                case Kind.INT_ESCAPE:
                    type = Type.ESCAPE;
                    break;
                case Kind.INT_QUOT_ESCAPE:
                    type = Type.QUOT_ESCAPE;
                    break;
                default:
                    throw new IllegalArgumentException(
                        "Invalid delimiter type: " + rawDelim.getKind());
            }
        }
        if (rawDelim.isSetPrecedence()) {
            prec = (byte) rawDelim.getPrecedence();
        }
        if (!rawDelim.isSetOptionalMode()) {
            lieu = Mode.NEVER;
        } else {
            switch (rawDelim.getOptionalMode().intValue()) {
                case OptionalMode.INT_ALLOW:
                    lieu = Mode.ALLOW;
                    break;
                case OptionalMode.INT_FAVOR:
                    lieu = Mode.FAVOR;
                    break;
                case OptionalMode.INT_FORCE:
                    lieu = Mode.FORCE;
                    break;
                case OptionalMode.INT_NEVER:
                    lieu = Mode.NEVER;
                    break;
                default:
                    throw new IllegalArgumentException(
                        "Invalid optional mode: " + rawDelim.getOptionalMode());
            }
        }
        if (!rawDelim.isSetTerminatorMode()) {
            term = Mode.NEVER;
        } else {
            switch (rawDelim.getTerminatorMode().intValue()) {
                case TerminatorMode.INT_ALLOW:
                    term = Mode.ALLOW;
                    break;
                case TerminatorMode.INT_FAVOR:
                    term = Mode.FAVOR;
                    break;
                case TerminatorMode.INT_FORCE:
                    term = Mode.FORCE;
                    break;
                case TerminatorMode.INT_NEVER:
                    term = Mode.NEVER;
                    break;
                default:
                    throw new IllegalArgumentException(
                        "Invalid terminator mode: " + rawDelim.getTerminatorMode());
            }
        }
        byte[] beginBytes = null;
        byte[] endBytes = null;
        short beginSlot = -1;
        short endSlot = -1;
        if (rawDelim.isSetBytes()) {
            if (rawDelim.getBytes().isSetConstant()) {
                //static delimiter
                endBytes =
                    Misc.str2bytes(
                            Misc.nonPrintable(rawDelim.getBytes().getConstant()));
            } else {
                int pos = rawDelim.getBytes().getEmbedded().getOffset();
                int len = rawDelim.getBytes().getEmbedded().getLength();
                endSlot = (short) slotList.size();
                slotList.add(new Delim.Slot(pos, len));
            }
        }
        if (rawDelim.isSetBeginBytes()) {
            if (rawDelim.getBeginBytes().isSetConstant()) {
                beginBytes =
                    Misc.str2bytes(
                            Misc.nonPrintable(
                                    rawDelim.getBeginBytes().getConstant()));
            } else {
                int pos = rawDelim.getBeginBytes().getEmbedded().getOffset();
                int len = rawDelim.getBeginBytes().getEmbedded().getLength();
                beginSlot = (short) slotList.size();
                slotList.add(new Delim.Slot(pos, len));
            }
        }
        if (beginBytes == null && endBytes == null && beginSlot == -1
                && endSlot == -1) {
            throw new EncoderConfigurationException(
                    "Delimiter must have either begin bytes "
                    + "or end bytes specified. " + rawDelim.xmlText());
        }
        return new Delim(type, prec, term, lieu, endSlot, endBytes,
                rawDelim.isSetSkipLeading() && rawDelim.getSkipLeading(),
                rawDelim.isSetCollapse() && rawDelim.getCollapse(),
                beginSlot, beginBytes,
                rawDelim.isSetBeginAnch() ? rawDelim.getBeginAnch() : true,
                rawDelim.isSetEndAnch() ? rawDelim.getEndAnch() : true);
    }

    /**
     * Loads delimiter levels (two dimensional array) from the custom
     * properties.
     *
     * @param nodeProps the custom properties
     * @param delimList the current delimiter list
     * @param slotList the current slot list
     * @return the delimiter levels in two dimentional array, which can be
     *          used by <code>Nodes.add(...)</code>
     * @throws EncoderConfigurationException
     */
    private static int[][] getLevels(NodeProperties nodeProps,
            List<Delim> delimList, List<Delim.Slot> slotList)
            throws EncoderConfigurationException {
        if (!nodeProps.isSetDelimiterSet()) {
            return null;
        }
        DelimiterLevel[] rawLevels =
            nodeProps.getDelimiterSet().getLevelArray();
        int[][] levels = new int[rawLevels.length][];
        Delimiter[] rawDelims;
        int[] delims;
        for (int i = 0; i < rawLevels.length; i++) {
            rawDelims = rawLevels[i].getDelimiterArray();
            delims = new int[rawDelims.length];
            for (int j = 0; j < rawDelims.length; j++) {
                delims[j] = delimList.size();
                delimList.add(loadDelimiter(rawDelims[j], slotList));
            }
            levels[i] = delims;
        }
        return levels;
    }

    /**
     * Creates and populates a raw node from the custom properties, which
     * in turn can be used to create an instance of <code>Nodes.Node</code>.
     *
     * @param parentKey the node key of the parent node
     * @param indexWithinParent the child index within the parent node
     * @param schemaElem the element declaration
     * @param nodeMap the map mapping from node key to node index, maybe mutated.
     * @param nodeList the current raw node list, maybe mutated.
     * @param delimList the current delimiter list
     * @param slotList the current slot list
     * @param contentModelMap maps contentModel to a list of subnode indices
     * @return index to the node being loaded.
     * @throws EncoderConfigurationException thrown if any invalidity is found
     *              in the metadata (the XSD plus &lt;appinfo&gt; extension)
     */
    private static int loadRawNode(String parentKey, int indexWithinParent,
        SchemaLocalElement schemaElem, Map<String, Integer> nodeMap,
        List<RawNode> nodeList, List<Delim> delimList,
        List<Delim.Slot> slotList,
        Map<SchemaParticle, List<Integer>> contentModelMap)
        throws EncoderConfigurationException {

        // Form the key. For a root element, e.g.:
        // "{http://xml.netbeans.org/schema/customenc}mylist". For non-root
        // element, e.g.: "{http://xml.netbeans.org/schema/customenc}Root|0"
        String key;
        if (indexWithinParent == -1) {
            // must be root element.
            key = schemaElem.getName().toString();
        } else {
            // must be non-root element.
            key = parentKey + "|" + indexWithinParent;
        }
        // Check nodeMap to see if the element has been processed
        Integer nodeIndex = nodeMap.get(key);
        if (nodeIndex != null) {
            // i.e. this element has been processed, so simply return index.
            return nodeIndex.intValue();
        }

        // i.e. this element hasn't been processed, so build the raw node
        SchemaType schemaType = schemaElem.getType();
        if (schemaType == null || schemaType.isNoType()
           || (schemaType.isURType() && !schemaType.isSimpleType())) {
            throw new EncoderConfigurationException(
                "No type or UR type is not supported.  element='"
                + schemaElem.getName() + "'");
        }
        CustomEncoding customEncoding;
        NodeProperties nodeProps;
        try {
            customEncoding = getCustomEncoding(schemaElem);
        } catch (XmlException ex) {
            throw new EncoderConfigurationException(
                    "Unable to load node properties.", ex);
        }
        if (customEncoding == null) {
            customEncoding = CustomEncoding.Factory.newInstance();
            nodeProps = customEncoding.addNewNodeProperties();
            nodeProps.setNodeType(NodeProperties.NodeType.DELIMITED);
        } else {
            nodeProps = customEncoding.getNodeProperties();
            if (nodeProps == null) {
                nodeProps = NodeProperties.Factory.newInstance();
                nodeProps.setNodeType(NodeProperties.NodeType.DELIMITED);
            }
        }
        // flag indicating if simple content or simple type.
        boolean isSimple = (schemaType.getContentType() == SchemaType.SIMPLE_CONTENT
                || schemaType.isSimpleType());

        Node.Type nodeType = getNodeType(nodeProps);
        Node.Order order = getOrder(nodeProps);
        int length = (nodeType == Node.Type.FIXED ? nodeProps.getLength() : 0);
        Node.Align align = getAlign(nodeProps);
        byte[] match = isSimple ? getMatch(nodeProps) : null;
        List<Integer> subs = null;
        int[][] levels = getLevels(nodeProps, delimList, slotList);
        String antecoding = null;
        if (nodeProps.isSetInputCharset()) {
            antecoding = nodeProps.getInputCharset();
        }
        String decoding = null;
        if (nodeProps.isSetParsingCharset()) {
            decoding = nodeProps.getParsingCharset();
        }
        String encoding = null;
        if (nodeProps.isSetSerializingCharset()) {
            encoding = nodeProps.getSerializingCharset();
        }
        String postcoding = null;
        if (nodeProps.isSetOutputCharset()) {
            postcoding = nodeProps.getOutputCharset();
        }
        String preDecodeCharCoding = null;
        if (customEncoding.isSetPreDecodeCharCoding()) {
            preDecodeCharCoding = customEncoding.getPreDecodeCharCoding();
        }
        String postEncodeCharCoding = null;
        if (customEncoding.isSetPostEncodeCharCoding()) {
            postEncodeCharCoding = customEncoding.getPostEncodeCharCoding();
        }
        if (nodeType == Node.Type.GROUP) {
            //Check what kind of group it is
            if (isSimple || schemaType.getContentModel() == null) {
                throw new EncoderConfigurationException(
                        "Group node must not be simple type (content)."
                        + "  element='" + schemaElem.getName() + "'");
            }
            if (schemaType.getContentModel().getParticleType()
                    == SchemaParticle.CHOICE) {
                nodeType = Node.Type.ALTER;
            }
        }
        int offset = -1;
        if (nodeProps.isSetOffset()) {
            offset = (int) nodeProps.getOffset();
        }
        int position = -1;
        if (nodeProps.isSetPosition()) {
            position = (int) nodeProps.getPosition();
        }
        String defaultValue = null;
        if (schemaElem.isDefault()) {
            defaultValue = schemaElem.getDefaultText();
        }
        String scavengerChars = null;
        if (nodeProps.isSetScvngr()) {
            scavengerChars = nodeProps.getScvngr().getChars();
        }
        boolean output1stChars = false;
        if (nodeProps.isSetScvngr() && nodeProps.getScvngr().isSetEmit1St()) {
            output1stChars = nodeProps.getScvngr().getEmit1St();
        }
        int minNOfChildren = -1;
        if (nodeProps.isSetNOfN() && nodeProps.getNOfN().isSetMinN()) {
            minNOfChildren = nodeProps.getNOfN().getMinN();
        }
        int maxNOfChildren = -1;
        if (nodeProps.isSetNOfN() && nodeProps.getNOfN().isSetMaxN()) {
            maxNOfChildren = nodeProps.getNOfN().getMaxN();
        }
        // consolidate the minOccurs, encoding node's minOcc overrides
        // element's minOccurs
        int minOccurs = 1;
        if (nodeProps.isSetMinOcc()) {
            minOccurs = (int) nodeProps.getMinOcc();
        } else if (schemaElem.getMinOccurs() != null) {
            minOccurs = schemaElem.getMinOccurs().intValue();
        }
        // consolidate the maxOccurs, encoding node's minOcc overrides
        // element's maxOccurs
        int maxOccurs = -1;
        if (nodeProps.isSetMaxOcc()) {
            maxOccurs = (int) nodeProps.getMaxOcc();
        } else {
            if (schemaElem instanceof SchemaGlobalElement) {
                maxOccurs = 1;
            } else if (schemaElem.getMaxOccurs() != null) {
                maxOccurs = schemaElem.getMaxOccurs().intValue();
            }
        }
        Node.UndefDataPolicy undefDataPolicy = Node.UndefDataPolicy.PROHIBIT;
        if (nodeProps.isSetUndefDataPolicy()) {
            if (UndefDataPolicy.SKIP == nodeProps.getUndefDataPolicy()) {
                undefDataPolicy = Node.UndefDataPolicy.SKIP;
            } else if (UndefDataPolicy.MAP == nodeProps.getUndefDataPolicy()) {
                undefDataPolicy = Node.UndefDataPolicy.MAP;
            }
        }
        boolean fineGrainedInherit = false;
        if (nodeProps.isSetFineInherit()) {
            fineGrainedInherit = nodeProps.getFineInherit();
        }
        boolean noMatch = false;
        if (nodeProps.isSetNoMatch()) {
            noMatch = nodeProps.getNoMatch();
        }
        DelimForFixedLen delimForFixedLen = null;
        if (nodeProps.isSetDelimOfFixed()) {
            byte[] beginBytes = Misc.str2bytes(
                Misc.nonPrintable(
                nodeProps.getDelimOfFixed().getBeginBytes()));
            boolean beginAnch = false;
            if (nodeProps.getDelimOfFixed().isSetBeginAnch()) {
                beginAnch = nodeProps.getDelimOfFixed().getBeginAnch();
            }
            delimForFixedLen = new DelimForFixedLen(beginBytes, beginAnch);
        }
        byte[] escapeSequence = null;
        if (nodeProps.isSetEscapeSequence()) {
            escapeSequence = Misc.str2bytes(
                    Misc.nonPrintable(nodeProps.getEscapeSequence()));
        }
        // now constructing a RawNode instance
        RawNode rawNode;
        {
            int fromNodeId = -1;
            int toNodeId = -1;
            String namespace = schemaElem.getName().getNamespaceURI();
            String localName = schemaElem.getName().getLocalPart();
            int elmType = SchemaType.NOT_SIMPLE;
            if (schemaType.getSimpleVariety() == SchemaType.ATOMIC) {
                elmType = schemaType.getBuiltinTypeCode();
            }
            rawNode = new RawNode(fromNodeId, toNodeId, nodeType, namespace,
                    localName, indexWithinParent,
                    order, length, align, match, subs, levels, antecoding,
                    decoding, encoding, postcoding, preDecodeCharCoding,
                    postEncodeCharCoding, offset, position, defaultValue,
                    scavengerChars, output1stChars, minNOfChildren,
                    maxNOfChildren, minOccurs, maxOccurs, undefDataPolicy,
                    fineGrainedInherit, noMatch, delimForFixedLen,
                    escapeSequence, elmType);
        }
        nodeIndex = new Integer(nodeList.size()); // keep it incremental
        nodeList.add(rawNode);
        nodeMap.put(key, nodeIndex);

        if (isSimple) {
            return nodeIndex.intValue();
        }

        // it must be not a simple content nor a simple type.
        SchemaParticle contentModel = schemaType.getContentModel();
        if (contentModel == null) {
            throw new EncoderConfigurationException(
                "Complex type without child element is not supported."
                + "  element='" + schemaElem.getName() + "'");
        }
        if (contentModel.getParticleType() == SchemaParticle.WILDCARD) {
            throw new EncoderConfigurationException(
                "Wildcard is not supported for this encoding style."
                + "  element='" + schemaElem.getName() + "'");
        }

        // check contentModelMap to see if contentModel already there. Support
        // recursion: https://open-esb.dev.java.net/issues/show_bug.cgi?id=1350
        List<Integer> subnodes = contentModelMap.get(contentModel);
        if (subnodes == null) {
            // i.e. contentModel has never been encountered before
            int len, i;
            if (contentModel.getParticleType() == SchemaParticle.ELEMENT) {
                // must be that the model group only has one child
                len = 1;
                i = len - 1;
                subnodes = new ArrayList<Integer>(len);
                contentModelMap.put(contentModel, subnodes);
                int idx = loadRawNode(key, i, (SchemaLocalElement) contentModel,
                    nodeMap, nodeList, delimList, slotList, contentModelMap);
                subnodes.add(idx);
            } else {
                // must be model group
                SchemaParticle[] particles = contentModel.getParticleChildren();
                len = particles.length;
                subnodes = new ArrayList<Integer>(len);
                contentModelMap.put(contentModel, subnodes);
                for (i = 0; i < len; i++) {
                    if (particles[i].getParticleType() != SchemaParticle.ELEMENT) {
                        throw new EncoderConfigurationException(
                            "Amonymous group is not supported." + "  element='"
                            + schemaElem.getName() + "'");
                    }
                    int idx = loadRawNode(key, i, (SchemaLocalElement) particles[i],
                        nodeMap, nodeList, delimList, slotList, contentModelMap);
                    subnodes.add(idx);
                }
            }
        }
        rawNode.mSubnodes = subnodes;

        return nodeIndex.intValue();
    }

    /**
     * Immutable descriptor for a single raw node used in loading metadata
     * from an XSD.
     */
    private static class RawNode {

        public final int mFromId; // obsolete
        public final int mNodeId; //obsolete
        public final String mNamespace; // namespace from XSD
        public final String mName; // local name from an element declaration
        public final Node.Type mType; // from Node.Type
        public final int[][] mLevel; // delim levels
        public final int mChildIdx; // field number below parent
        public List<Integer> mSubnodes; // list of sub-nodes, null if none
        public final Node.Order mOrder; // from Node.Order
        public final int mLength; // for fixed field; <= 0 means no limit
        public final Node.Align mAlign; // from Node.Align
        public final byte[] mMatch; // exact input match sequence
        public final String mAntecoding;
        public final String mEncoding;
        public final String mDecoding;
        public final String mPostcoding;
        public final String mPreDecodeCharCoding;
        public final String mPostEncodeCharCoding;
        public final int mOffset;
        public final int mPosition;
        public final String mDefaultValue;
        public final String mScavengerChars;
        public final boolean mOutput1stChar;
        public final int mMinNOfChildren;
        public final int mMaxNOfChildren;
        public final int mMinOccurs;
        public final int mMaxOccurs;
        public final Node.UndefDataPolicy mUndefDataPolicy;
        public final boolean mFineGrainedInherit;
        public final boolean mNoMatch;
        public final DelimForFixedLen mDelimForFixedLen;
        public final byte[] mEscapeSequence;
        public final int mElmType;

        /**
         * Creates descriptor from given attributes.
         *
         * @param from  the parent node's Fog ID, for tracing (obsolete)
         * @param to  the child node's Fog ID, for tracing (obsolete)
         * @param type  the node type, from Node.Type
         * @param namespace namespace of the node
         * @param localName  local name of the node
         * @param child  field number below parent
         * @param order  the child ordering on input, from Node.Order
         * @param length  for limited fixed fields, else use 0
         * @param align  the match rule algorithm, from Node.Align
         * @param match  exact input match sequence, null if none
         * @param subs  the sub-nodes, as indices in Node.mNodes[], null if none
         * @param level  local delimiter list, or null if none
         * @param antecoding input encoding charset for decodeFromBytes(...)
         * @param decoding parsing decoding charset
         * @param encoding serializing encoding charset
         * @param postcoding output encoding charset for encodeToBytes(...)
         * @param preDecodeCharCoding char coding to convert char seq to
         *          byte seq before decoding for decodeFromString(...)
         * @param postEncodeCharCoding char coding to convert byte seq to
         *          char seq after encoding for encodeToString(...)
         * @param offset offset of fixed length field
         * @param position offset from the current position to the position
         *                  where actual length can be found
         * @param defaultValue default value of non-optional field
         * @param scavengerChars scavenger characters
         * @param output1stChar flag indicates if first character of the
         *              scavenger characters should be marshaled out into
         *              output.
         * @param minNOfChildren minimum number of children must have data
         * @param maxNOfChildren maximum number of children must have data
         * @param minOccurs minimum number of occurrences, only for overriding
         *                  the minOccurs in XSD element declaration.
         * @param maxOccurs maximum number of occurrences, -1 if no limit.
         *                  only for overriding the minOccurs in XSD element
         *                  declaration.
         * @param undefDataPolicy policy for handling undefined trailing data
         * @param fineGrainedInherit if the delimiters should be inherited in
         *                      a fine grained fashion, which means begin, end
         *                      and repeating delimiters can be inherited
         *                      separately.
         * @param noMatch if the match condition should be reverted.
         * @param delimForFixedLen simple delimiter used by fixed length
         *               fields to skip some bytes before getting the value.
         * @param escapeSequence the global level escape sequence.
         * @param elmType element schema type.
         */
        public RawNode(int from, int to, Node.Type type, String namespace,
            String localName, int childIdx, Node.Order order,
            int length, Node.Align align, byte[] match, List<Integer> subs,
            int[][] level, String antecoding, String decoding, String encoding,
            String postcoding, String preDecodeCharCoding,
            String postEncodeCharCoding, int offset, int position,
            String defaultValue, String scavengerChars, boolean output1stChar,
            int minNOfChildren, int maxNOfChildren, int minOccurs,
            int maxOccurs, Node.UndefDataPolicy undefDataPolicy,
            boolean fineGrainedInherit,
            boolean noMatch, DelimForFixedLen delimForFixedLen,
            byte[] escapeSequence, int elmType) {

            mFromId = from;
            mNodeId = to;
            mType = type;
            mNamespace = namespace;
            mName = localName;
            mChildIdx = childIdx;
            mOrder  = order;
            mLength = length;
            mAlign = align;
            mMatch = match;
            mSubnodes = subs;
            mLevel = level;
            mAntecoding = antecoding;
            mDecoding = decoding;
            mEncoding = encoding;
            mPostcoding = postcoding;
            mPreDecodeCharCoding = preDecodeCharCoding;
            mPostEncodeCharCoding = postEncodeCharCoding;
            mOffset = offset;
            mPosition = position;
            mDefaultValue = defaultValue;
            mScavengerChars = scavengerChars;
            mOutput1stChar = output1stChar;
            mMinNOfChildren = minNOfChildren;
            mMaxNOfChildren = maxNOfChildren;
            mMinOccurs = minOccurs;
            mMaxOccurs = maxOccurs;
            mUndefDataPolicy = undefDataPolicy;
            mFineGrainedInherit = fineGrainedInherit;
            mNoMatch = noMatch;
            mDelimForFixedLen = delimForFixedLen;
            mEscapeSequence = escapeSequence;
            mElmType = elmType;
        }

        @Override
        public String toString() {
            StringBuffer sb = new StringBuffer();
            sb.append("RawNode@").append(Integer.toHexString(hashCode()));
            sb.append(" name[").append(mName).append("]");
            sb.append(" type[").append(mType).append("]");
            sb.append(" childIdx[").append(mChildIdx).append("]");
            sb.append(" order[").append(mOrder).append("]");
            sb.append(" length[").append(mLength).append("]");
            sb.append(" align[").append(mAlign).append("]");
            if (mMatch != null) {
                sb.append(" match[").append(mMatch).append("]");
            }
            if (mFromId > -1) {
                sb.append(" from[").append(mFromId).append("]");
            }
            if (mNodeId > -1) {
                sb.append(" to[").append(mNodeId).append("]");
            }
            if (mSubnodes != null && mSubnodes.size() > 0) {
                sb.append(" subnodes[");
                for (int i = 0; i < mSubnodes.size(); i++) {
                    sb.append(mSubnodes.get(i)).append(",");
                }
                sb.append("]");
            }

            return sb.toString();
        }
    }
}
