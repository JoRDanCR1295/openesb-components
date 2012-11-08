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
 * @(#)CocoXsdBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.xsdbuilder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.impl.xb.xsdschema.Element;
import org.apache.xmlbeans.impl.xb.xsdschema.Group;
import org.apache.xmlbeans.impl.xb.xsdschema.LocalComplexType;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;
import org.apache.xmlbeans.impl.xb.xsdschema.AppinfoDocument.Appinfo;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;
import org.apache.xmlbeans.impl.xb.xsdschema.FormChoice.Enum;

import com.sun.encoder.coco.CocoEncoderProvider;
import com.sun.encoder.coco.appinfo.CocoEncoding;
import com.sun.encoder.coco.appinfo.CocoEncodingMark;
import com.sun.encoder.coco.appinfo.CocoEncoding.OccursDependOn;
import com.sun.encoder.coco.model.CocoDataModel;
import com.sun.encoder.coco.model.CocoDescriptionEntry;
import com.sun.encoder.coco.model.CocoLexer;
import com.sun.encoder.coco.model.CocoParser;
import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;
import com.sun.encoder.frmwk.appinfo.EncodingMark.Encoding;

/**
 * COBOL Copybook - XSD builder implementation.
 * 
 * @author  Noel Ang, Jun Xu
 * @version $Revision: 1.6 $
 */
public class CocoXsdBuilder {

    /**
     * Builder version, embedded in generated XSDs 
     */ 
    public static final String BUILDER_VERSION;
    static {
        String ver = "$Revision: 1.6 $";
        int pos1 = ver.lastIndexOf(':') + 2;
        int pos2 = ver.lastIndexOf('$') - 1;
        if (pos1 < pos2) {
            ver = ver.substring(pos1, pos2).trim();
        }
        BUILDER_VERSION = ver;
    }

    private static final boolean mShowTrace =
        "true".equals(System.getProperty("sun.encoder.coco.showtrace"));
    private CocoXsdBuilderSpec mSpec;
    private File mCopybookFile;
    private String mCopybookCharEncoding;
    private File mXsdFile;
    private String mTargetNamespace;
    private boolean mIgnoreExtraContent;
    private boolean mCheckReservedWord;
    private String mDisplayCharEncoding;
    private String mDisplay1CharEncoding;
    private final Map<CocoDescriptionEntry, SchemaNode> mEntriesToNodes;
    private final ErrorManager mErrorMgr =
            ErrorManager.getManager("OpenESB.encoder.COBOLCopybook."
                                    + getClass().getName());
    
    /**
     * Create COBOL Copybook - XSD builder instance
     */
    public CocoXsdBuilder() {
        mCopybookFile = null;
        mXsdFile = null;
        mTargetNamespace   = null;
        mSpec      = null;
        mEntriesToNodes = Collections.synchronizedMap(
                new HashMap<CocoDescriptionEntry, SchemaNode>(10000));
    }
    
    /**
     * Create COBOL Copybook - XSD builder instance
     *
     * @param spec Builder Spec {@link CocoXsdBuilderSpec context}
     */
    public CocoXsdBuilder(CocoXsdBuilderSpec spec) {
        mCopybookFile = null;
        mXsdFile = null;
        mTargetNamespace   = null;
        validateSpec(spec);
        mSpec = spec;
        mEntriesToNodes = Collections.synchronizedMap(
                new HashMap<CocoDescriptionEntry, SchemaNode>(10000));
    }

    /**
     * Set builder context.
     *
     * @param spec context
     */
    public void setOtdBuilderSpec(CocoXsdBuilderSpec spec) {
        validateSpec(spec);
        mSpec = (CocoXsdBuilderSpec) spec;
    }
    
    /**
     * Retrieve builder context.
     *
     * @return CocoXsdBuilderSpec context object associated with builder
     */
    public CocoXsdBuilderSpec getOtdBuilderSpec() {
        return mSpec;
    }
    
    /**
     * Check context for correctness. If a correctable datum is found, it is
     * corrected quietly and no exception is thrown.
     *
     * @param  spec context in consideration
     *
     * @throws java.lang.IllegalArgumentException if context fails to validate
     */
    private void validateSpec(CocoXsdBuilderSpec spec)
            throws IllegalArgumentException {
        Message msg;
        String text;
        
        if (spec == null) {
            return;
        }

        /* COBOL Copybook input: non-null; must exist; must be readable file */
        String copybookLocation = spec.getCopybookLocation();
        if (copybookLocation == null) {
            msg = MessageCatalog.getMessage("CCCB4012");
            text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        
        File copybookFile = new File(copybookLocation);
        if (!copybookFile.canRead() || !copybookFile.isFile()) {
            msg = MessageCatalog.getMessage("CCCB4015");
            text = msg.formatText(new Object[]{copybookFile.getAbsolutePath()});
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
    
        /* COBOL Copybook file encoding: non-null */
        String copybookEncoding = spec.getCopybookCharEncoding();
        if (copybookEncoding == null) {
            msg = MessageCatalog.getMessage("CCCB4013");
            text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        
        /* XSD file location: must not be null.  Must a valid file path */
        String xsdLocation = spec.getXsdLocation();
        if (xsdLocation == null) {
            msg = MessageCatalog.getMessage("CCCB4014");
            text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
    
        /* XSD target namespace: might be null, otherwise must be a valid URI */
        String targetNamespace = spec.getTargetNamespace();
        if (targetNamespace != null) {
            try {
                new URI(targetNamespace);
            } catch (URISyntaxException e) {
                msg = MessageCatalog.getMessage("CCCB4017");
                text = msg.formatText(new Object[] { targetNamespace } );
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new IllegalArgumentException(text);
            }
        }
        
        /* Character encoding for DISPLAY usage fields */
        String displayEncoding = spec.getDisplayCharEncoding();
        if (displayEncoding == null) {
            msg = MessageCatalog.getMessage("CCCB4020");
            text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        
        /* Character encoding for DISPLAY1 usage fields */
        String display1Encoding = spec.getDisplay1CharEncoding();
        if (display1Encoding == null) {
            msg = MessageCatalog.getMessage("CCCB4021");
            text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalArgumentException(text);
        }
        
        mCopybookFile = copybookFile;
        mCopybookCharEncoding = copybookEncoding;
        mXsdFile = new File(xsdLocation);
        mTargetNamespace   = targetNamespace;
        mIgnoreExtraContent = spec.getIgnoreContentBeyondCol72();
        mCheckReservedWord = spec.getCheckNamesForReservedWords();
        mDisplayCharEncoding = displayEncoding;
        mDisplay1CharEncoding = display1Encoding;
    }
    
    /**
     * Build the XSD document. A partial document may be built should this
     * method fail to successfully complete.
     *
     * @param xsdDocument XSD document
     *
     * @throws CocoXsdBuilderException if any error occurs that prevents the
     *          successful building of the XSD
     *
     * @throws java.lang.IllegalStateException if the OTD cannot be built due
     *          to insufficient information (e.g., context not yet given)
     */
    public void buildXsd()
            throws CocoXsdBuilderException, IllegalStateException {
    
        if (mSpec == null) {
            Message msg = MessageCatalog.getMessage("CCCB4018");
            String text = msg.toString();
            mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
            throw new IllegalStateException(text);
        }
    
        try {
            /* Build intermediate model of COCOCO data */
            CocoLexer lexer =
                new CocoLexer(mCopybookFile, mCopybookCharEncoding);
            lexer.setDisable72ColumnLimit(!mIgnoreExtraContent);
    
            CocoParser parser = new CocoParser(lexer);
            parser.disableItemNameReservedWordChecking(!mCheckReservedWord);
            CocoDataModel model = null;
            model = parser.parse();
    
            /* Output parser trace info if wanted */
            if (mShowTrace && mXsdFile != null) {
                File tFile =
                    File.createTempFile("CocoXsdBuilder", "debug", mXsdFile);
                PrintStream outStream =
                    new PrintStream(new FileOutputStream(tFile));
                model.toStream(outStream);
                outStream.close();
            }
    
            SchemaDocument schemaDoc = SchemaDocument.Factory.newInstance();
            Schema schema = schemaDoc.addNewSchema();
            if (mTargetNamespace != null && mTargetNamespace.length() > 0) {
                schema.setTargetNamespace(mTargetNamespace);
            }
            createXsd(model, schema);
            schema.setElementFormDefault(Enum.forString("qualified"));
            XmlOptions options = new XmlOptions();
            options.setSaveAggressiveNamespaces();
            Map<String, String> prefixes = new HashMap<String, String>();
            prefixes.put("urn:com.sun:encoder", "enc");
            prefixes.put("urn:com.sun:encoder-coco-1.0", "coco");
            options.setSaveSuggestedPrefixes(prefixes);
            options.setSavePrettyPrint();
            options.setSavePrettyPrintIndent(4);
            options.setSavePrettyPrintOffset(0);
            options.setCharacterEncoding("UTF-8");
            schemaDoc.save(mXsdFile, options);
        } catch (Exception e) {
            throw new CocoXsdBuilderException(e.getLocalizedMessage(), e);
        }
    }
    
    /**
     * Populate OTD container with tree built from data model.
     *
     * @param model     COBOL Copybook data model
     * @param schema the XML schema
     *
     * @throws CocoXsdBuilderException if any error occurs that prevents
     *          the successful building of the tree
     */
    private void createXsd(CocoDataModel model, Schema schema)
            throws CocoXsdBuilderException {
    
        populateEncodingMark(schema);
    
        growXsd(model.getRoot(), new SchemaNode(schema, null, null, null));
    }
    
    /**
     * Populate encoding mark with builder parameters
     *
     * @param  schema XML Schema to insert encoding mark to
     */
    private void populateEncodingMark(Schema schema) {
        CocoEncodingMark encodingMark = CocoEncodingMark.Factory.newInstance();
        Encoding encoding = encodingMark.addNewEncoding();
        encoding.setName("COBOL Copybook Encoder");
        encoding.setNamespace("urn:com.sun:encoder-coco-1.0");
        encoding.setStyle(CocoEncoderProvider.STYLE_ID);
        encodingMark.setCocoXsdBuilderVendor("SUNW");
        encodingMark.setCocoXsdBuilderVersion(BUILDER_VERSION);
        encodingMark.setCopybookCharEncoding(mCopybookCharEncoding);
        String inputFilename = null;
        try {
            inputFilename = mCopybookFile.getCanonicalPath();
        } catch (IOException ie) {
            inputFilename = mCopybookFile.getAbsolutePath();
        }
        encodingMark.setCopybookLocation(inputFilename);
        encodingMark.setIgnoreContentBeyondCol72(mIgnoreExtraContent);
        encodingMark.setCheckNamesForReservedWords(mCheckReservedWord);
        if (mXsdFile != null) {
            String outputFileName = null;
            try {
                outputFileName = mXsdFile.getCanonicalPath();
            } catch (IOException ie2) {
                outputFileName = mXsdFile.getAbsolutePath();
            }
            encodingMark.setXsdLocation(outputFileName);
        }
        Appinfo appInfo = schema.addNewAnnotation().addNewAppinfo();
        appInfo.set(encodingMark);
        appInfo.setSource("urn:com.sun:encoder");
    }
    
    /**
     * Populate the element with data model information.
     *
     * @param  entry List of model elements to process; elements are
     *         processed FIFO-like, that is from the head on down; list
     *         should be "shallow" and contain sibling entries of the
     *         immediate model tree level in regard
     *
     * @param  base the schema node to grow
     * @throws CocoXsdBuilderException 
     */
    private void growXsd(CocoDescriptionEntry entry, SchemaNode base)
            throws CocoXsdBuilderException {
    
        SchemaNode newNode = growNode(entry, base);
        if (!entry.isElementary()) {
            for (int i = 0; i < entry.countChildren(); i++) {
                growXsd(entry.getChild(i), newNode);
            }
        }
        plantRedefines(entry, newNode, base);
        plantOccursDependsOn(entry, newNode);
    }
    
    /**
     * Populate the tree with a model entry's redefinitions.
     *
     * @param  entry Model entry in consideration
     *
     * @param  entryElem element of the redefined object (entry)
     *
     * @param  base  the schema or element on which to plant the redefinitions
     * @throws CocoXsdBuilderException 
     * @throws IndexOutOfBoundsException 
     */
    private void plantRedefines(CocoDescriptionEntry entry,
            SchemaNode entryElement, SchemaNode base)
            throws IndexOutOfBoundsException, CocoXsdBuilderException {
    
        for (int i = 0; i < entry.countRedefinitions(); i++) {
    
            CocoDescriptionEntry redef = entry.getRedefinition(i);
            SchemaNode node = growNode(redef, base);
    
            setRedefine(node, entryElement.getElement());
    
            /*
             * plant redefine's children
             */
            for (int c = 0; c < redef.countChildren(); c++) {
                growXsd(redef.getChild(c), node);
            }
        }
    }

    private void setRedefine(SchemaNode redefineSubject,
            Element redefinedObject) {
        redefineSubject.getEncoding().setRedefine(redefinedObject.getName());
        redefineSubject.getElement().getAnnotation().getAppinfoArray(0).set(
                redefineSubject.getEncoding());
        XmlCursor cursor = redefinedObject.newCursor();
        cursor.setAttributeText(new QName("minOccurs"), "0");
        cursor.dispose();
        cursor = redefineSubject.getElement().newCursor();
        cursor.setAttributeText(new QName("minOccurs"), "0");
        cursor.dispose();
    }
    
    private void setOccursDependOn(SchemaNode entryNode,
            SchemaNode nodeBeingDepended) {
        CocoEncoding encoding = entryNode.getEncoding();
        OccursDependOn occurs = encoding.addNewOccursDependOn();
        QName qName =
            new QName(nodeBeingDepended.getSchema().getTargetNamespace(),
                    nodeBeingDepended.getTopElement().getName());
        occurs.setTopElement(qName);
        occurs.setPath(nodeBeingDepended.getPath());
        entryNode.getElement().getAnnotation().getAppinfoArray(0).set(encoding);
    }
    
    /**
     * Store the reference information of an entry's depends-on entry into
     * the entry element's annotation 
     *
     * @param  entry depends-on's <em>subject</em> entry description
     *
     * @param  entryElement <code>entry's</code> element
     */
    private void plantOccursDependsOn(final CocoDescriptionEntry entry, 
            final SchemaNode entryElement) throws CocoXsdBuilderException {

        final CocoDescriptionEntry depends = entry.getOccursOn();
        SchemaNode refNode;
        
        if (null != depends) {
            refNode = mEntriesToNodes.get(depends);
            if (null != refNode) {
                setOccursDependOn(entryElement, refNode);
            } else {
                Message msg = MessageCatalog.getMessage("CCCB4102");
                String text = msg.formatText(new Object[]{
                    depends.getName(),
                    entryElement.getElement().getName()
                });
                mErrorMgr.log(ErrorManager.Severity.ERROR, null, text);
                throw new CocoXsdBuilderException(text);
            }
        }
    }
    
    Group getSequenceGroup(Element elem) {
        LocalComplexType cType;
        if (elem.isSetComplexType()) {
            cType = elem.getComplexType();
        } else {
            cType = elem.addNewComplexType();
        }
        if (cType.isSetSequence()) {
            return cType.getSequence(); 
        } else {
            return cType.addNewSequence();
        }
    }
    
    /**
     * Grow a node for a data model entry.
     *
     * @param  entry The data model entry to process
     *
     * @param  base  The schema node to grow leaf
     *
     * @return the newly created element
     */
    private SchemaNode growNode(CocoDescriptionEntry entry, SchemaNode base) {

        String name = entry.getName();
        Element elem = null;
        boolean isTop = (base.getElement() == null);
        if (isTop) {
            elem = base.getSchema().addNewElement();
            base.setTopElement(elem);
        } else {
            elem = getSequenceGroup(base.getElement()).addNewElement();
        }
        elem.setName(name);
        if (!isTop) {
            XmlCursor cursor = elem.newCursor();
            cursor.toLastAttribute();
            cursor.toNextToken();
            cursor.insertAttributeWithValue(new QName("minOccurs"),
                    String.valueOf(entry.getMinimumOccurs()));
            cursor.insertAttributeWithValue(new QName("maxOccurs"),
                    String.valueOf(entry.getMaximumOccurs()));
            cursor.dispose();
        }
        
        CocoEncoding encoding = defineElement(entry, elem);
        if (isTop) {
            encoding.setTop(true);
            if (mSpec.getPreDecodeCharCoding() != null) {
                encoding.setPreDecodeCharCoding(
                        mSpec.getPreDecodeCharCoding());
            }
            if (mSpec.getPostEncodeCharCoding() != null) {
                encoding.setPostEncodeCharCoding(
                        mSpec.getPostEncodeCharCoding());
            }
            encoding.setDisplayCharEncoding(mDisplayCharEncoding);
            encoding.setDisplay1CharEncoding(mDisplay1CharEncoding);
            elem.getAnnotation().getAppinfoArray(0).set(encoding);
        }
        SchemaNode node;
        if (base.getElement() == null) {
            node = new SchemaNode(base.getSchema(), elem, "", encoding);
        } else {
            node =
                new SchemaNode(base.getSchema(), elem,
                        base.getPath().length() == 0 ? name
                                : base.getPath() + "/" + name,
                        encoding);
        }
        node.setTopElement(base.getTopElement());
        mEntriesToNodes.put(entry, node);
        return node;
    }
    
    /**
     * Encode data model entry into serial overlay.
     *
     * @param  entry The data model entry in consideration
     *
     * @param  elem The element representation of the entry
     */
    private CocoEncoding defineElement(CocoDescriptionEntry entry,
            Element elem) {
        return entry.toElement(elem);
    }
    
    private class SchemaNode {
        
        private final Schema mSchema;
        private final Element mElem;
        private final String mPath;
        private final CocoEncoding mEncoding;
        private Element mTopElement;
        
        public SchemaNode(Schema schema, Element elem, String path,
                CocoEncoding encoding) {
            mSchema = schema;
            mElem = elem;
            mPath = path;
            mEncoding = encoding;
        }
        
        public Schema getSchema() {
            return mSchema;
        }
        
        public Element getElement() {
            return mElem;
        }
        
        public String getPath() {
            return mPath;
        }
        
        public CocoEncoding getEncoding() {
            return mEncoding;
        }
        
        public void setTopElement(Element topElem) {
            mTopElement = topElem;
        }
        
        public Element getTopElement() {
            return mTopElement;
        }
    }
}
