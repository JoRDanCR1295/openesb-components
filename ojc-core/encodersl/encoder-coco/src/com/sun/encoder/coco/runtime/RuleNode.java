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
 * @(#)RuleNode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaAnnotation;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument;

import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.coco.appinfo.CocoEncoding;
import com.sun.encoder.coco.appinfo.CocoEncoding.Usage;
import com.sun.encoder.coco.model.CocoDescriptionEntry;
import com.sun.encoder.coco.model.CocoPicture;
import com.sun.encoder.coco.model.CocoDescriptionEntry.UsageType;
import com.sun.encoder.coco.model.CocoPicture.Category;
import com.sun.encoder.coco.runtime.messages.ErrorManager;
import com.sun.encoder.coco.runtime.messages.Message;
import com.sun.encoder.coco.runtime.messages.MessageCatalog;
import com.sun.encoder.codegen.Emit;

/**
 * Node that represents a COBOL Copybook entry.  This node class is deemed for
 * runtime use (used by COBOL Copybook data parser and marshaler), so it is
 * compact and all getters should return pre-calculated values as much as
 * possible.
 * 
 * @author Jun Xu
 * @since 6.0
 */
public class RuleNode {

    private static final ErrorManager cErrorMgr =
        ErrorManager.getManager("OpenESB.encoder.COBOLCopybook."
                                + RuleNode.class.getName());
    
    private boolean mSuper = false;
    private boolean mTop = false;
    private RuleContext mContext;
    private RuleNode mParentNode;
    /** Child index used to get this node from its parent node. */
    private int mIndex;
    private QName mQName;
    private int mMinOccurs;
    private int mMaxOccurs;
    private RuleNode mOccursDependOn;
    private String mCharEncoding;

    /**
     * mChildList is only used when initially populating the rule tree. Once
     * this is done, only mChildren should be used.
     */
    private List<RuleNode> mChildList = new ArrayList<RuleNode>();
    private RuleNode[] mChildren;

    private CobolCharacteristics mCharacteristics;
    private String mPicture;

    /**
     * if the redefine index is greater or equal to zero, then this node has
     * been redefined.  Redefine index is used to retrieve data from the
     * redefined data pool.
     */
    private int mRedefineIndex = -1;

    /**
     * if the 'occurs depend' on index is greater or equal to zero, then this
     * node has been depended on for occurrence. Occurs Depending On index is
     * used to retrieve data from the Occurs Depending On data pool.
     */
    private int mOccursDependOnIndex = -1;

    /**
     * If the redefined node is not null, then this node is redefining another
     * node
     */
    private RuleNode mRedefinedNode;
    
    public RuleContext getContext() {
        return mContext;
    }
    
    public RuleNode getParentNode() {
        return mParentNode;
    }
    
    public int getIndex() {
        return mIndex;
    }
    
    public boolean isSuper() {
        return mSuper;
    }
    
    public boolean isTop() {
        return mTop;
    }
    
    public QName getQName() {
        return mQName;
    }
    
    public int getMinOccurs() {
        return mMinOccurs;
    }
    
    public int getMaxOccurs() {
        return mMaxOccurs;
    }
    
    public int getOccursDependOnIndex() {
        return mOccursDependOnIndex;
    }
    
    public RuleNode getOccursDependOn() {
        return mOccursDependOn;
    }
    
    public String getCharEncoding() {
        if (mCharEncoding != null) {
            return mCharEncoding;
        }
        if (mCharacteristics.getPicCategory()
                == CobolCharacteristics.PIC_DBCS) {
            return mContext.mDisplay1CharEncoding;
        }
        return mContext.mDisplayCharEncoding;
    }
    
    public int getRedefineIndex() {
        return mRedefineIndex;
    }
    
    public RuleNode getRedefinedNode() {
        return mRedefinedNode;
    }
    
    public RuleNode[] getChildren() {
        return mChildren;
    }
    
    public CobolCharacteristics getCharacteristics() {
        return mCharacteristics;
    }
    
    public String getPicture() {
        return mPicture;
    }
    
    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("RuleNode@")
                .append(Integer.toHexString(hashCode()));
        buf.append(" name='").append(mQName).append("'");
        if (mMinOccurs != -1) {
            buf.append(" minOccurs=").append(mMinOccurs);
        }
        if (mMaxOccurs != -1) {
            buf.append(" maxOccurs=").append(mMaxOccurs);
        }
        if (mCharEncoding != null) {
            buf.append(" charEncoding=").append(mCharEncoding);
        }
        if (mOccursDependOn != null) {
            buf.append(" occursDependOn=").append(mOccursDependOn.getQName());
        }
        if (mOccursDependOnIndex >= 0) {
            buf.append(" occursDependOnIndex=").append(mOccursDependOnIndex);
        }
        if (mRedefinedNode != null) {
            buf.append(" redefine=").append(mRedefinedNode.getQName());
        }
        if (mRedefineIndex >= 0) {
            buf.append(" redefineIndex=").append(mRedefineIndex);
        }
        if (mPicture != null) {
            buf.append(" picture=").append(mPicture);
        }
        if (mCharacteristics != null) {
            buf.append(" characteristics=").append(mCharacteristics);
        }
        if (mChildren != null && mChildren.length > 0) {
            buf.append(" children={");
            for (int i = 0; i < mChildren.length; i++) {
                buf.append(mChildren[i].getQName());
                if (i < mChildren.length -1) {
                    buf.append(", ");
                }
            }
            buf.append("}]");
        }
        return buf.toString();
    }
    
    public void dump(Emit emit) {
        emit.part("(name=");
        emitValue(emit, mQName);
        emit.down(" ");
        emit.indent();
        emit.part("minOccurs=");
        emitValue(emit, mMinOccurs);
        emit.emit();
        emit.part("maxOccurs=");
        emitValue(emit, mMaxOccurs);
        emit.emit();
        emit.part("charEncoding=");
        emitValue(emit, mCharEncoding);
        emit.emit();
        emit.part("occursDependOn=");
        emitValue(emit,
                mOccursDependOn == null ? "<null>"
                        : mOccursDependOn.getQName());
        emit.emit();
        emit.part("occursDependOnIndex=");
        emitValue(emit, mOccursDependOnIndex);
        emit.emit();
        emit.part("redefine=");
        emitValue(emit,
                mRedefinedNode == null ? "<null>" : mRedefinedNode.getQName());
        emit.emit();
        emit.part("redefineIndex=");
        emitValue(emit, mRedefineIndex);
        emit.emit();
        emit.part("picture=");
        emitValue(emit, mPicture);
        emit.emit();
        emit.part("characteristics=");
        emitValue(emit, mCharacteristics);
        emit.emit();
        emit.undent();
        for (int i = 0; mChildren != null && i < mChildren.length; i++) {
            mChildren[i].dump(emit);
        }
        emit.undent();
        emit.emit(")");
    }
    
    private void emitValue(Emit emit, Object value) {
        emit.part("\"");
        emit.part(value == null ? "<null>" : value.toString());
        emit.part("\"");
    }
    
    /**
     * Wraps a top node into a super node. A super node does not
     * represent any rule but just acts as a container of the top
     * node for facilitating programing logic in some situations.
     * 
     * @param topNode the top node
     * @return the super node that contains the top node as its child node
     */
    public static RuleNode wrapAsSuperNode(RuleNode topNode) {
        RuleNode superNode = new RuleNode();
        superNode.mSuper = true;
        superNode.mQName = new QName("super");
        superNode.mChildren = new RuleNode[] {topNode};
        superNode.mMaxOccurs = 1;
        superNode.mMinOccurs = 1;
        topNode.mParentNode = superNode;
        topNode.mIndex = 0;
        return superNode;
    }
    
    /**
     * Reads a rule tree from an XML type system (compiled from XSDs) and
     * a top element.
     * 
     * @param ts the XML type system
     * @param topElemName the qualified name of a top element
     * @return the top rule
     * @throws EncoderConfigurationException thrown if any configuration error
     *         occurs (e.g., invalidity in the XSD or its annotation) 
     */
    public static RuleNode readRules(SchemaTypeSystem ts, QName topElemName)
            throws EncoderConfigurationException {
        SchemaGlobalElement topElem = ts.findElement(topElemName);
        if (topElem == null) {
            //Global element ''{0}'' not found in the type system
            Message msg = MessageCatalog.getMessage("CCCR4033");
            String err = msg.formatText(new Object[] {topElemName});
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
            throw new EncoderConfigurationException(err);
        }
        
        RuleNode topRule = new RuleNode();
        topRule.mTop = true;
        RuleContext context = new RuleContext();
        context.mTopRule = topRule;
        topRule.mContext = context;
        readRule(topRule, topElem);
        context.mRedefinedNodes =
            context.mRedefinedNodeList.toArray(new RuleNode[0]);
        context.mRedefinedNodeList.clear();
        context.mOccursDependOnNodes =
            context.mOccursDependOnList.toArray(new RuleNode[0]);
        context.mOccursDependOnList.clear();
        return topRule;
    }

    /**
     * Reads current RuleNode.
     * @param currNode
     * @param elem
     * @throws EncoderConfigurationException
     */
    private static void readRule(RuleNode currNode,
            SchemaLocalElement elem) throws EncoderConfigurationException {
        RuleContext context = currNode.getContext();
        CocoEncoding encoding = null;
        SchemaAnnotation anno = elem.getAnnotation();
        if (anno != null) {
            XmlObject[] xmlObjs = anno.getApplicationInformation();
            for (int i = 0; i < xmlObjs.length; i++) {
                if (xmlObjs[i] instanceof CocoEncoding) {
                    encoding = (CocoEncoding) xmlObjs[i];
                    break;
                }
                XmlCursor cursor = xmlObjs[i].newCursor();
                String source = cursor.getAttributeText(new QName("source"));
                cursor.dispose();
                if ("urn:com.sun:encoder".equals(source)) {
                    Reader reader = xmlObjs[i].newReader();
                    try {
                        encoding = CocoEncoding.Factory.parse(reader);
                        reader.close();
                    } catch (XmlException e) {
                        //Missing encoding information 
                        Message msg = MessageCatalog.getMessage("CCCR4035");
                        String err =
                            msg.formatText(new Object[] {elem.getName()});
                        cErrorMgr.log(ErrorManager.Severity.ERROR, null,
                                err + e.getLocalizedMessage());
                        throw new EncoderConfigurationException(err, e);
                    } catch (IOException e) {
                        //Missing encoding information 
                        Message msg = MessageCatalog.getMessage("CCCR4035");
                        String err =
                            msg.formatText(new Object[] {elem.getName()});
                        cErrorMgr.log(ErrorManager.Severity.ERROR, null,
                                err + e.getLocalizedMessage());
                        throw new EncoderConfigurationException(err, e);
                    }
                    break;
                }
            }
        }
        if (encoding == null) {
            //Missing encoding information 
            Message msg = MessageCatalog.getMessage("CCCR4034");
            String err = msg.formatText(new Object[] {elem.getName()});
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
            throw new EncoderConfigurationException(err);
        }
        currNode.mQName = elem.getName();
        if (elem instanceof SchemaGlobalElement) {
            currNode.mMinOccurs = 1;
            currNode.mMaxOccurs = 1;
        } else {
            currNode.mMinOccurs = elem.getMinOccurs().intValue();
            currNode.mMaxOccurs =
                elem.getMaxOccurs() == null ? -1
                        : elem.getMaxOccurs().intValue();
        }
        if (currNode.mTop) {
            if (encoding.isSetDisplayCharEncoding()) {
                currNode.mContext.mDisplayCharEncoding =
                    encoding.getDisplayCharEncoding();
            }
            if (encoding.isSetDisplay1CharEncoding()) {
                currNode.mContext.mDisplay1CharEncoding =
                    encoding.getDisplay1CharEncoding();
            }
            if (encoding.isSetPreDecodeCharCoding()) {
                currNode.mContext.mPreDecodeCharCoding =
                    encoding.getPreDecodeCharCoding();
            }
            if (encoding.isSetPostEncodeCharCoding()) {
                currNode.mContext.mPostEncodeCharCoding =
                    encoding.getPostEncodeCharCoding();
            }
        }
        CobolCharacteristics characteristics = new CobolCharacteristics();
        currNode.mCharacteristics = characteristics;
        characteristics.setBlankWhenZero(encoding.isSetBlankWhenZero());
        characteristics.setJustified(encoding.isSetJustified());
        if (encoding.isSetRedefine()) {
            if (currNode.mTop) {
                Message msg = MessageCatalog.getMessage("CCCR4036");
                String err = msg.formatText(new Object[] {elem.getName()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                throw new EncoderConfigurationException(err);
            }
            RuleNode redefinedNode =
                findSibling(currNode, encoding.getRedefine());
            if (redefinedNode == null) {
                Message msg = MessageCatalog.getMessage("CCCR4037");
                String err =
                    msg.formatText(new Object[] {elem, encoding.getRedefine()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                throw new EncoderConfigurationException(err);
            }
            if (redefinedNode.mRedefineIndex == -1) {
                redefinedNode.mRedefineIndex =
                    context.mRedefinedNodeList.size();
                context.mRedefinedNodeList.add(redefinedNode);
            }
            currNode.mRedefinedNode = redefinedNode;
        }
        if (encoding.isSetOccursDependOn()) {
            RuleNode dependedNode =
                findNode(context.mTopRule,
                        encoding.getOccursDependOn().getPath());
            if (dependedNode == null) {
                Message msg = MessageCatalog.getMessage("CCCR4038");
                String err =
                    msg.formatText(new Object[]{
                            elem, encoding.getOccursDependOn().getPath()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                throw new EncoderConfigurationException(err);
            }
            if (dependedNode.mOccursDependOnIndex == -1) {
                dependedNode.mOccursDependOnIndex =
                    context.mOccursDependOnList.size();
                context.mOccursDependOnList.add(dependedNode);
            }
            currNode.mOccursDependOn = dependedNode;
        }
        CocoPicture picture;
        if (encoding.isSetPicture()) {
            try {
                picture = new CocoPicture(encoding.getPicture());
            } catch (IllegalArgumentException e) {
                Message msg = MessageCatalog.getMessage("CCCR4039");
                String err =
                    msg.formatText(new Object[] {elem, encoding.getPicture()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null,
                        err + e.getLocalizedMessage());
                throw new EncoderConfigurationException(err, e);
            }
            characteristics.setDecimalPosition(picture.getDecimalPosition());
            characteristics.setDecimalScalingPositions(
                    picture.getDecimalScalingPositions());
            characteristics.setSigned(picture.hasSign());
            characteristics.setPicCategory(
                    cvtPictureCategory(picture.getCategory()));
            currNode.mPicture = picture.getPicture();
        } else {
            picture = null;
        }
        if (encoding.isSetUsage()) {
            characteristics.setUsage(cvtUsage(encoding.getUsage()));
        }
        characteristics.setSignLeading(characteristics.isSigned()
                && encoding.isSetSign() && encoding.getSign().isSetLeading());
        characteristics.setSignSeparate(characteristics.isSigned()
                && encoding.isSetSign() && encoding.getSign().isSetSeparate());
        
        SchemaType xmlType = elem.getType();
        if (xmlType == null || xmlType.isURType()) {
            Message msg = MessageCatalog.getMessage("CCCR4040");
            String err = msg.formatText(new Object[] {elem.getName()});
            cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
            throw new EncoderConfigurationException(err);
        }
        if (xmlType.isSimpleType()) {
            characteristics.setSize(
                    CocoDescriptionEntry.computeElementarySize(
                            picture,
                            cvtUsage(characteristics.getUsage()),
                            characteristics.isSignSeparate()));
        } else {
            SchemaParticle model = xmlType.getContentModel();
            if (model == null
                    || model.getParticleType() == SchemaParticle.WILDCARD
                    || (model.getParticleType() != SchemaParticle.ELEMENT
                            && model.countOfParticleChild() == 0)) {
                Message msg = MessageCatalog.getMessage("CCCR4041");
                String err = msg.formatText(new Object[] {elem.getName()});
                cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                throw new EncoderConfigurationException(err);
            }
            SchemaParticle[] parts;
            if (model.getParticleType() == SchemaParticle.ELEMENT) {
                parts = new SchemaParticle[]{model};
            } else { 
                parts  = xmlType.getContentModel().getParticleChildren();
            }
            int size = 0;
            for (int i = 0; i < parts.length; i++) {
                if (!(parts[i] instanceof SchemaLocalElement)) {
                    Message msg = MessageCatalog.getMessage("CCCR4042");
                    String err =
                        msg.formatText(new Object[] {elem, new Integer(i)});
                    cErrorMgr.log(ErrorManager.Severity.ERROR, null, err);
                    throw new EncoderConfigurationException(err);
                }
                RuleNode childNode = new RuleNode();
                currNode.mChildList.add(childNode);
                childNode.mParentNode = currNode;
                childNode.mIndex = i;
                childNode.mContext = context;
                readRule(childNode, (SchemaLocalElement) parts[i]);
                int occurs;
                if (childNode.getOccursDependOn() != null) {
                    //Same logic in CocoDescriptionEntry's getSize() method
                    occurs = 1;
                } else {
                    occurs = childNode.getMaxOccurs();
                }
                size += (occurs * childNode.getCharacteristics().getSize());
            }
            characteristics.setSize(size);
            currNode.mChildren = currNode.mChildList.toArray(new RuleNode[0]);
        }
    }
    
    private static RuleNode findSibling(RuleNode current, String localName) {
        if (current.mParentNode.mChildList == null) {
            return null;
        }
        for (RuleNode node : current.mParentNode.mChildList) {
            if (node.getQName().getLocalPart().equals(localName)) {
                return node;
            }
        }
        return null;
    }
    
    private static RuleNode findNode(RuleNode top, String path) {
        if (path == null || path.length() == 0) {
            return top;
        }
        String[] steps = path.split("/");
        RuleNode current = top;
        RuleNode found = null;
        for (int i = 0; i < steps.length; i++) {
            found = null;
            if (current.mChildList == null) {
                break;
            }
            for (RuleNode node : current.mChildList) {
                if (steps[i].equals(node.getQName().getLocalPart())) {
                    found = node;
                    break;
                }
            }
            if (found != null) {
                current = found;
            } else {
                break;
            }
        }
        return found;
    }
    
    /**
     * Converts picture category enumeration value from
     * <code>CocoPicture</code>'s definition to
     * <code>CobolCharacteristics<code>' definition.
     * 
     * @param cocoPictureCategory picture category enumeration value as defined
     *                            in <code>CocoPicture</code>
     * @return picture enumeration category value as defined in
     *          <code>CobolCharacteristics</code>
     */
    private static int cvtPictureCategory(Category cocoPictureCategory) {
        switch(cocoPictureCategory) {
        case ALPHABETIC:
            return CobolCharacteristics.PIC_ALPHA;
        case ALPHANUMERIC:
            return CobolCharacteristics.PIC_ALPHANUM;
        case ALPHANUMERIC_EDITED:
            return CobolCharacteristics.PIC_ALPHANUME;
        case DBCS:
            return CobolCharacteristics.PIC_DBCS;
        case EX_FLOAT:
            return CobolCharacteristics.PIC_EXFLOAT;
        case NUMERIC:
            return CobolCharacteristics.PIC_NUM;
        case NUMERIC_EDITED:
            return CobolCharacteristics.PIC_NUME;
        default:
            return CobolCharacteristics.PIC_DEGENERATE;
        }
    }
    
    /**
     * Converts usage from XSD annotation form to CobolCharacteristics'
     * usage form.
     * 
     * @param usage usage as defined in COBOL Copybook XSD
     * @return usage enumeration value as defined in
     *              <code>CobolCharacteristics</code> 
     */
    private static int cvtUsage(Usage usage) {
        if (usage.isSetBinary()) {
            return CobolCharacteristics.USAGE_BINARY;
        }
        if (usage.isSetComp() || usage.isSetComputational()) {
            return CobolCharacteristics.USAGE_COMP;
        }
        if (usage.isSetComp1() || usage.isSetComputational1()) {
            return CobolCharacteristics.USAGE_COMP1;
        }
        if (usage.isSetComp2() || usage.isSetComputational2()) {
            return CobolCharacteristics.USAGE_COMP2;
        }
        if (usage.isSetComp3() || usage.isSetComputational3()) {
            return CobolCharacteristics.USAGE_COMP3;
        }
        if (usage.isSetComp4() || usage.isSetComputational4()) {
            return CobolCharacteristics.USAGE_COMP4;
        }
        if (usage.isSetComp5() || usage.isSetComputational5()) {
            return CobolCharacteristics.USAGE_COMP5;
        }
        if (usage.isSetDisplay()) {
            return CobolCharacteristics.USAGE_DISPLAY;
        }
        if (usage.isSetDisplay1()) {
            return CobolCharacteristics.USAGE_DISPLAY1;
        }
        if (usage.isSetIndex()) {
            return CobolCharacteristics.USAGE_INDEX;
        }
        if (usage.isSetPackedDecimal()) {
            return CobolCharacteristics.USAGE_PACKED;
        }
        return CobolCharacteristics.USAGE_DEGENERATE;
    }
    
    /**
     * Converts usage from CobolCharacteristics usage form form to
     * CocoDescriptionEntry.UsageType form.
     * 
     * @param usage usage enumeration value as defined in
     *              <code>CobolCharacteristics</code>
     * @return usage usage enumeration value as defined in
     *          <code>CocoDescriptionEntry.UsageType</code>
     */
    private static int cvtUsage(int usage) {
        switch (usage) {
        case CobolCharacteristics.USAGE_BINARY:
            return UsageType.BINARY;
        case CobolCharacteristics.USAGE_COMP:
            return UsageType.COMP;
        case CobolCharacteristics.USAGE_COMP1:
            return UsageType.COMP1;
        case CobolCharacteristics.USAGE_COMP2:
            return UsageType.COMP2;
        case CobolCharacteristics.USAGE_COMP3:
            return UsageType.COMP3;
        case CobolCharacteristics.USAGE_COMP4:
            return UsageType.COMP4;
        case CobolCharacteristics.USAGE_COMP5:
            return UsageType.COMP5;
        case CobolCharacteristics.USAGE_DISPLAY:
            return UsageType.DISPLAY;
        case CobolCharacteristics.USAGE_DISPLAY1:
            return UsageType.DISPLAY1;
        case CobolCharacteristics.USAGE_INDEX:
            return UsageType.INDEX;
        case CobolCharacteristics.USAGE_PACKED:
            return UsageType.PACDEC;
        }
        return UsageType.DISPLAY;
    }
    
    public static class RuleContext {
        private RuleNode mTopRule;
        private List<RuleNode> mRedefinedNodeList = new ArrayList<RuleNode>();
        private List<RuleNode> mOccursDependOnList = new ArrayList<RuleNode>();
        private RuleNode[] mRedefinedNodes;
        private RuleNode[] mOccursDependOnNodes;
        private String mDisplayCharEncoding;
        private String mDisplay1CharEncoding;
        private String mPreDecodeCharCoding;
        private String mPostEncodeCharCoding;
        
        public RuleNode getTopRule() {
            return mTopRule;
        }
        
        public RuleNode[] getRedefinedNodes() {
            return mRedefinedNodes;
        }
        
        public RuleNode[] getOccursDependOnNodes() {
            return mOccursDependOnNodes;
        }
        
        public String getPreDecodeCharCoding() {
            return mPreDecodeCharCoding;
        }
        
        public String getPostEncodeCharCoding() {
            return mPostEncodeCharCoding;
        }
    }
    
    public static void main(String[] argv) {
        try {
            String xsdLocation = "test/data/redefcob2a.xsd";
            SchemaDocument schemaDoc =
                SchemaDocument.Factory.parse(new File(xsdLocation));
            SchemaTypeSystem ts =
                XmlBeans.compileXsd(
                        new XmlObject[]{schemaDoc},
                        XmlBeans.getContextTypeLoader(),
                        null);
            RuleNode topRule = readRules(ts, new QName("TESTCOB"));
            StringWriter writer = new StringWriter();
            topRule.dump(new Emit(writer, 0, 4));
            System.out.println(writer.toString());
        } catch (XmlException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (EncoderConfigurationException e) {
            e.printStackTrace();
        } 
    }
}
