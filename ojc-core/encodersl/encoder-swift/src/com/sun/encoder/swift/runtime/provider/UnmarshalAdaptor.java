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
 * @(#)UnmarshalAdaptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.runtime.provider;

import com.sun.encoder.tools.xml.SchemaLocationAttributes;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.QNameSet;
import org.apache.xmlbeans.QNameSetBuilder;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

import com.sun.encoder.swift.runtime.LexicalException;
import com.sun.encoder.swift.runtime.provider.SwiftCharEscapeCoder.Result;
import java.net.URL;

/**
 * This class implements an unmarshal adaptor for decoding Swift standard
 * encoding rule encoded data.  It implements SAX XMLReader interface, but
 * the input source is not in XML format but Swift standard encoding rule
 * encoded.  The adaptor translates Swift standard encoding rule encoded data
 * into SAX events.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public final class UnmarshalAdaptor implements XMLReader {
    
    //Namespace URI for extension components (Z segments or extra fields,
    //components, subcomponents at the end
    public static final String NS_URI_FOR_EXTENSIONS =
        "urn:com.sun.encoder.hl7.ns.extentions";
    
    //Semantic level.  !!These values should NOT be changed because
    //in the program ++ and -- might be used to get new values for level.
    private static final int DOCUMENT = 0;
    private static final int SEGMENT = 1;
    private static final int FIELD = 2;
    private static final int COMPONENT = 3;
    private static final int SUBCOMPONENT = 4;
    private static final int MAX_SEMANTIC_LEVELS = 5;
    private static final int MAX_MODEL_LEVELS = 50;
    
    private static final int KIND_SIMPLE = 1;
    private static final int KIND_ESCAPE = 2;
    private static final int KIND_VARIES = 4;
    private static final int KIND_COMPLEX = 8;
    private static final int KIND_ELEMENTARY = 7;

    //buffer capacity settings
    private static final int DEFAULT_BUF_CAPACITY = 1024;
    private static final int MINIMUM_BUF_CAPACITY = 12;
    private static final int MAXIMUM_BUF_CAPACITY = 131072;
    
    private static final EmptyAttributes mEmptyAttributes =
        new EmptyAttributes();
    
    private static QNameSet mQNameSetForExtensions;
    static {
        QNameSetBuilder qnSetBuilder = new QNameSetBuilder();
        qnSetBuilder.addNamespace(NS_URI_FOR_EXTENSIONS);
        mQNameSetForExtensions = qnSetBuilder.toQNameSet();
    }
    
    //delimiter masks
    private static int[] mDelimQualiMask = new int[MAX_SEMANTIC_LEVELS];
    private static int[] mDelimEndMask = new int[MAX_SEMANTIC_LEVELS];
    static {
        mDelimEndMask[SEGMENT] = Token.SEG_TERM; 
        mDelimQualiMask[FIELD] = Token.DELIM_NOT_READ | Token.SEG_TERM
            | Token.FIELD_SEP | Token.REPET_SEP;
        mDelimEndMask[FIELD] = Token.SEG_TERM | Token.FIELD_SEP; 
        mDelimQualiMask[COMPONENT] = Token.DELIM_NOT_READ | Token.SEG_TERM
            | Token.FIELD_SEP | Token.REPET_SEP | Token.COMPO_SEP;
        mDelimEndMask[COMPONENT] = Token.SEG_TERM | Token.FIELD_SEP
            | Token.REPET_SEP | Token.COMPO_SEP;
        mDelimQualiMask[SUBCOMPONENT] = Token.DELIM_NOT_READ | Token.SEG_TERM
            | Token.FIELD_SEP | Token.REPET_SEP | Token.COMPO_SEP
            | Token.SUBCOMPO_SEP;
        mDelimEndMask[SUBCOMPONENT] = Token.SEG_TERM
            | Token.FIELD_SEP | Token.REPET_SEP | Token.COMPO_SEP
            | Token.SUBCOMPO_SEP;
    }
    
    //map between delimiter type and semantic level
    private static int[] mDelimType2Level = new int[Token.SUBCOMPO_SEP + 1];
    static {
        Arrays.fill(mDelimType2Level, -1);
        mDelimType2Level[Token.SEG_TERM] = SEGMENT;
        mDelimType2Level[Token.FIELD_SEP] = FIELD;
        mDelimType2Level[Token.REPET_SEP] = FIELD;
        mDelimType2Level[Token.COMPO_SEP] = COMPONENT;
        mDelimType2Level[Token.SUBCOMPO_SEP] = SUBCOMPONENT;
    }
    
    //private static int mCallCount = 0;
    
    private final URL mSchemaLocation;
    private final SchemaGlobalElement mElement;
    private final int mBufferCapacity;
    
    private String mGroupNamePrefix;
    private EntityResolver mEntityResolver;
    private DTDHandler mDTDHandler;
    private ContentHandler mContentHandler;
    private ErrorHandler mErrorHandler;
    
    
    /** Creates a new instance of UnmarshalAdaptor */
    public UnmarshalAdaptor(URL schemaLocation, SchemaGlobalElement element) {
        this(schemaLocation, element, DEFAULT_BUF_CAPACITY);
    }
    
    public UnmarshalAdaptor(URL schemaLocation, SchemaGlobalElement element, int bufferCapacity) {
        if (element == null) {
            throw new NullPointerException("no global element.");
        }
        mSchemaLocation = schemaLocation;
        mElement = element;
        mBufferCapacity = bufferCapacity;
        mGroupNamePrefix = element.getName().getLocalPart() + ".";
    }

    public boolean getFeature(String name)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        if ("http://xml.org/sax/features/namespaces".equals(name)) {
            return true;
        }
        if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
            return false;
        }
        throw new SAXNotRecognizedException();
    }

    public void setFeature(String name, boolean value)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        if ("http://xml.org/sax/features/namespaces".equals(name)) {
            if (!value) {
                throw new SAXNotSupportedException(
                        "Feature '" + name + "' with value " + value
                        + " is not supported.");
            }
            return;
        }
        if ("http://xml.org/sax/features/namespace-prefixes".equals(name)) {
            if (value) {
                throw new SAXNotSupportedException(
                        "Feature '" + name + "' with value " + value
                        + " is not supported.");
            }
            return;
        }
        throw new SAXNotRecognizedException();
    }

    public Object getProperty(String name)
            throws SAXNotRecognizedException, SAXNotSupportedException {
        return null;
    }

    public void setProperty(String name, Object value)
            throws SAXNotRecognizedException, SAXNotSupportedException {
    }

    public void setEntityResolver(EntityResolver resolver) {
        mEntityResolver = resolver;
    }

    public EntityResolver getEntityResolver() {
        return mEntityResolver;
    }

    public void setDTDHandler(DTDHandler handler) {
        mDTDHandler = handler;
    }

    public DTDHandler getDTDHandler() {
        return mDTDHandler;
    }

    public void setContentHandler(ContentHandler handler) {
        mContentHandler = handler;
    }

    public ContentHandler getContentHandler() {
        return mContentHandler;
    }

    public void setErrorHandler(ErrorHandler handler) {
        mErrorHandler = handler;
    }

    public ErrorHandler getErrorHandler() {
        return mErrorHandler;
    }

    public String getGroupNamePrefix() {
        return mGroupNamePrefix;
    }

    public void setGroupNamePrefix(String groupNamePrefix) {
        mGroupNamePrefix = groupNamePrefix;
    }

    public void parse(InputSource input) throws IOException, SAXException {
        if (mContentHandler == null) {
            throw new NullPointerException("Missing content handler.");
        }
        Lexer lexer = new Lexer(input, mBufferCapacity);
        mContentHandler.startDocument();
        mContentHandler.startPrefixMapping("xsi", "http://www.w3.org/2001/XMLSchema-instance");
        mContentHandler.startElement(
                mElement.getName().getNamespaceURI(),
                mElement.getName().getLocalPart(),
                mElement.getName().getLocalPart(),
                new SchemaLocationAttributes(
                    mElement.getName().getNamespaceURI(), mSchemaLocation));
        ParsingState state =
            new ParsingState(mElement, mContentHandler, lexer,
                    mGroupNamePrefix);
        state.mPublicId = input.getPublicId();
        state.mSystemId = input.getSystemId();
        state.mSemanticLevel = DOCUMENT;
        state.mModelLevel = 0;
        state.mQNamePath[0] = mElement.getName();
        state.mPendingStartTagAt = -1;
        state.mMinFactor[state.mModelLevel] = 1;
        lexer.fillNextToken(state.mToken);
        if (state.mToken.mTokenType == Token.SEG_NAME) {
            state.mSegName = new String(state.mToken.mChars,
                    state.mToken.mOffset,
                    state.mToken.mCount);
        }
        parseParticle(mElement.getType().getContentModel(), state, 1);
        mContentHandler.endElement(
                mElement.getName().getNamespaceURI(),
                mElement.getName().getLocalPart(),
                mElement.getName().getLocalPart());
        mContentHandler.endPrefixMapping("xsi");
        mContentHandler.endDocument();
    }

    public void parse(String systemId) throws IOException, SAXException {
        parse(new InputSource(systemId));
    }
    
    private static boolean parseParticle(SchemaParticle particle,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        if (state.mToken.mTokenType == Token.EOF) {
            return false;
        }
        switch (particle.getParticleType()) {
        case SchemaParticle.ELEMENT:
            int i = 0;
            QName qName;
            if (state.mSemanticLevel > SEGMENT) {
                int kind = whatKind(particle.getType());
                if ((kind & KIND_ELEMENTARY) != 0) {
                    if (!state.mToken.mIsConsumed) {
                        final int max = particle.getMaxOccurs() != null
                            ? particle.getIntMaxOccurs()
                                : Integer.MAX_VALUE;
                        final int semLevel = state.mSemanticLevel;
                        int nonQualiConsumed = 0;
                        while ((i < max)
                                && state.mToken.mTokenType == Token.VALUE) {
                            if (nonQualiConsumed > 0) {
                                //the previous delimiter is not an expected
                                //one and the token value must have been
                                //consumed
                                if (state.mToken.mTokenType == Token.VALUE
                                        && state.mToken.mDelimType
                                            == Token.DELIM_NOT_READ) {
                                    state.mLexer.fillNextToken(state.mToken);
                                    continue;
                                } else if ((state.mToken.mDelimType
                                        & mDelimQualiMask[semLevel]) != 0) {
                                    nonQualiConsumed = 0;
                                    state.mToken.mIsConsumed = true;
                                }
                                if ((state.mToken.mDelimType
                                        & mDelimEndMask[semLevel]) != 0) {
                                    break;
                                }
                                forwardToken(state, semLevel);
                                continue;
                            } else {
                                if ((state.mToken.mDelimType
                                        & mDelimQualiMask[semLevel]) == 0) {
                                    //The delimiter type is not expected at
                                    //this location. But always pick up the
                                    //first one even though the delimiter is
                                    //not as expected.  This is to allow old
                                    //version processor to process newer
                                    //version messages.
                                    ////For elementary content, need to see a
                                    ////qualified delimiter for the specific level
                                    //throwException("The delimiter is not"
                                    //        + " expected at the location.", null,
                                    //        state);
                                    nonQualiConsumed++;
                                }
                            }
                            if (state.mToken.mDelimType == Token.DELIM_NOT_READ
                                    && state.mToken.mCount == 0) {
                                state.mLexer.fillNextToken(state.mToken);
                                if (state.mToken.mTokenType != Token.VALUE
                                        || (state.mToken.mDelimType
                                            & mDelimQualiMask[semLevel]) == 0) {
                                    break;
                                }
                                if (state.mToken.mDelimType
                                        == Token.DELIM_NOT_READ
                                            && state.mToken.mCount == 0) {
                                    //This should not be possible
                                    throwException(
                                            "Consecutively no data read.",
                                            null, state);
                                }
                            }
                            if (state.mToken.mCount > 0) {
                                if (state.mPendingStartTagAt >= 0) {
                                    //fire the start element events that
                                    //are pending
                                    for (int j = state.mPendingStartTagAt;
                                            j < state.mModelLevel; j++) {
                                        qName = state.mQNamePath[j];
                                        state.mContentHandler.startElement(
                                            qName.getNamespaceURI(),
                                            qName.getLocalPart(),
                                            qName.getLocalPart(),
                                            mEmptyAttributes);
                                    }
                                    state.mPendingStartTagAt = -1;
                                }
                                qName = particle.getName();
                                state.mContentHandler.startElement(
                                        qName.getNamespaceURI(),
                                        qName.getLocalPart(),
                                        qName.getLocalPart(),
                                        mEmptyAttributes);
                                fireCharacters(state, false);
                                state.mToken.mIsConsumed = true;
                                while (state.mToken.mTokenType == Token.VALUE
                                        && state.mToken.mDelimType
                                            == Token.DELIM_NOT_READ) {
                                    state.mLexer.fillNextToken(state.mToken);
                                    if (state.mToken.mTokenType
                                            == Token.VALUE) {
                                        fireCharacters(state, false);
                                    }
                                    state.mToken.mIsConsumed = true;
                                }
                                state.mContentHandler.endElement(
                                        qName.getNamespaceURI(),
                                        qName.getLocalPart(),
                                        qName.getLocalPart());
                                i++;
                            } else {
                                state.mToken.mIsConsumed = true;
                            }
                            
                            if ((state.mToken.mDelimType
                                    & mDelimEndMask[semLevel]) != 0) {
                                break;
                            }
                            forwardToken(state, semLevel);
                        }
                    }
                    if (i < particle.getIntMinOccurs()
                            && state.mMinFactor[state.mModelLevel] > 0) {
                        throwException("Element: '"
                                + particle.getName().getLocalPart()
                                + "' must occur at least "
                                + particle.getIntMinOccurs()
                                + " time(s).", null, state);
                    }
                    while ((state.mToken.mDelimType
                            & mDelimEndMask[state.mSemanticLevel]) == 0) {
                        //Haven't seen the terminator for this level
                        forwardToken(state, state.mSemanticLevel);
                    }
                    forwardToken(state, state.mSemanticLevel);
                    return i > 0;
                } else {
                    if (state.mSemanticLevel == SUBCOMPONENT) {
                        throwException(
                                "Sub component must have simple content.",
                                null, state);
                    } else {
                        return parsePseudoGroup(particle, state, sequence);
                    }
                }
            } else if( state.mSemanticLevel == DOCUMENT) {
                if (particle.getName().getLocalPart().startsWith(
                        state.mGroupPrefix)) {
                    //a grouping element. Strip off the element and continue
                    //on the content model
                    return parsePseudoGroup(particle, state, sequence);
                } else {
                    state.mSemanticLevel++;
                    while (state.mToken.mTokenType == Token.SEG_NAME
                            && particle.getName().getLocalPart().equals(
                                    state.mSegName)) {
                        state.mMinFactor[state.mModelLevel] = 1;
                        if (state.mPendingStartTagAt >= 0) {
                            //fire the start element events that are pending
                            for (int j = state.mPendingStartTagAt;
                                    j < state.mModelLevel; j++) {
                                qName = state.mQNamePath[j];
                                state.mContentHandler.startElement(
                                        qName.getNamespaceURI(),
                                        qName.getLocalPart(),
                                        qName.getLocalPart(),
                                        mEmptyAttributes);
                            }
                            state.mPendingStartTagAt = -1;
                        }
                        qName = particle.getName();
                        state.mContentHandler.startElement(
                                qName.getNamespaceURI(),
                                qName.getLocalPart(),
                                qName.getLocalPart(),
                                mEmptyAttributes);
                        //segment, must be complex type
                        state.mLexer.fillNextToken(state.mToken);
                        parseParticle(particle.getType().getContentModel(),
                                state, sequence);
                        state.mContentHandler.endElement(
                                qName.getNamespaceURI(),
                                qName.getLocalPart(),
                                qName.getLocalPart());
                        //skip unrecognized fields
                        skipToNextSegment(state);
                        i++;
                    }
                    if (i < particle.getIntMinOccurs()
                            && state.mMinFactor[state.mModelLevel] > 0) {
                        throwException("Segment: '"
                                + particle.getName().getLocalPart()
                                + "' must occur at least "
                                + particle.getIntMinOccurs()
                                + " time(s).", null, state);
                    }
                    state.mSemanticLevel--;
                    return i > 0;
                }
            } else {
                //should not be possible unless programming error
                throw new IllegalStateException(
                        "Illegal semantic level: " + state.mSemanticLevel);
            }
        case SchemaParticle.SEQUENCE:
            final int seqChildCount = particle.countOfParticleChild();
            boolean seqResult = false; 
            if (seqChildCount > 0) {
                boolean mSemanticLevelIncreased = false;
                if (state.mSemanticLevel != DOCUMENT) {
                    state.mSemanticLevel++;
                    mSemanticLevelIncreased = true;
                }
                state.mModelLevel++;
                for (int j = 0; j < seqChildCount; j++) {
                    if (!seqResult
                            && state.mMinFactor[state.mModelLevel - 1] == 0) {
                        state.mMinFactor[state.mModelLevel] = 0;
                    } else {
                        state.mMinFactor[state.mModelLevel] = 1;
                    }
                    seqResult = (parseParticle(particle.getParticleChild(j),
                            state, j + 1)
                                || seqResult);
                    if (state.mToken.mIsConsumed) {
                        break;
                    }
                }
                state.mModelLevel--;
                if (mSemanticLevelIncreased) {
                    state.mSemanticLevel--;
                }
            }
            return seqResult;
        case SchemaParticle.CHOICE:
            final int choiceChildCount = particle.countOfParticleChild();
            boolean choiceResult = false; 
            if (choiceChildCount > 0) {
                boolean mSemanticLevelIncreased = false;
                if (state.mSemanticLevel != DOCUMENT) {
                    state.mSemanticLevel++;
                    mSemanticLevelIncreased = true;
                }
                state.mModelLevel++;
                state.mMinFactor[state.mModelLevel] = 0;
                for (int j = 0; j < choiceChildCount; j++) {
                    if (parseParticle(particle.getParticleChild(j),
                            state, j + 1)) {
                        choiceResult = true;
                        break;
                    }
                    if (state.mToken.mIsConsumed) {
                        break;
                    }
                }
                state.mModelLevel--;
                if (mSemanticLevelIncreased) {
                    state.mSemanticLevel--;
                }
            }
            return choiceResult;
        case SchemaParticle.ALL:
            final int allChildCount = particle.countOfParticleChild(); 
            boolean allResult = false; 
            if (allChildCount > 0) {
                boolean mSemanticLevelIncreased = false;
                if (state.mSemanticLevel != DOCUMENT) {
                    state.mSemanticLevel++;
                    mSemanticLevelIncreased = true;
                }
                state.mModelLevel++;
                state.mMinFactor[state.mModelLevel] = 0;
                boolean continueLoop = true;
                boolean[] found = new boolean[allChildCount];
                Arrays.fill(found, false);
                for (int j = 0; continueLoop && j < allChildCount; j++) {
                    continueLoop = false;
                    for (int k = 0; k < allChildCount; k++) {
                        if (parseParticle(particle.getParticleChild(k),
                                state, k + 1)) {
                            if (found[k]) {
                                throwException("Token not expected.",
                                        null, state);
                            } else {
                                found[k] = true;
                            }
                            allResult = true;
                            continueLoop = true;
                            break;
                        }
                        if (state.mToken.mIsConsumed) {
                            break;
                        }
                    }
                }
                for (int j = 0; j < allChildCount; j++) {
                    if (allResult 
                            && particle.getParticleChild(
                                    j).getIntMinOccurs() > 0
                            && !found[j]) {
                        throwException("Expected particle missing.",
                                null, state);
                    }
                }
                state.mModelLevel--;
                if (mSemanticLevelIncreased) {
                    state.mSemanticLevel--;
                }
            }
            return allResult;
        case SchemaParticle.WILDCARD:
            return parseWildcard(particle, state, sequence);
        default:
            //should not be possible unless programming error
            throw new IllegalStateException(
                    "Illegal particle type: " + particle.getParticleType());
        }
    }
    
    private static boolean parsePseudoGroup(SchemaParticle element,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        int i = 0;
        int min = element.getIntMinOccurs();
        if (element.getMaxOccurs() != null) {
            int max = element.getIntMaxOccurs();
            while (i < max) {
                if (state.mPendingStartTagAt == -1
                        || state.mPendingStartTagAt
                            > state.mModelLevel) {
                    state.mPendingStartTagAt = state.mModelLevel;
                }
                state.mQNamePath[state.mModelLevel] = element.getName();
                if (i >= min) {
                    state.mMinFactor[state.mModelLevel] = 0;
                }
                SchemaParticle subModel = element.getType().getContentModel();
                boolean levelIncreased = false;
                if (subModel.getParticleType() == SchemaParticle.ELEMENT
                        || subModel.getParticleType() == SchemaParticle.WILDCARD) {
                    state.mModelLevel++;
                    levelIncreased = true;
                }
                boolean result = parseParticle(subModel, state, sequence);
                if (levelIncreased) {
                    state.mModelLevel--;
                }
                if (result) {
                    state.mContentHandler.endElement(
                            element.getName().getNamespaceURI(),
                            element.getName().getLocalPart(),
                            element.getName().getLocalPart());
                    i++;
                } else {
                    break;
                }
                if (state.mSemanticLevel > SEGMENT) {
                    if ((state.mToken.mDelimType
                            & mDelimEndMask[state.mSemanticLevel]) != 0) {
                        break;
                    }
                    forwardToken(state, state.mSemanticLevel);
                }
            }
        } else {
            while (true) {
                if (state.mPendingStartTagAt == -1
                        || state.mPendingStartTagAt
                            > state.mModelLevel) {
                    state.mPendingStartTagAt = state.mModelLevel;
                }
                state.mQNamePath[state.mModelLevel] = element.getName();
                if (i >= min) {
                    state.mMinFactor[state.mModelLevel] = 0;
                }
                SchemaParticle subModel = element.getType().getContentModel();
                boolean levelIncreased = false;
                if (subModel.getParticleType() == SchemaParticle.ELEMENT
                        || subModel.getParticleType() == SchemaParticle.WILDCARD) {
                    state.mModelLevel++;
                    levelIncreased = true;
                }
                boolean result = parseParticle(subModel, state, sequence);
                if (levelIncreased) {
                    state.mModelLevel--;
                }
                if (result) {
                    state.mContentHandler.endElement(
                            element.getName().getNamespaceURI(),
                            element.getName().getLocalPart(),
                            element.getName().getLocalPart());
                    i++;
                } else {
                    break;
                }
                if (state.mSemanticLevel > SEGMENT) {
                    if ((state.mToken.mDelimType
                            & mDelimEndMask[state.mSemanticLevel]) != 0) {
                        break;
                    }
                    forwardToken(state, state.mSemanticLevel);
                }
            }
        }
        if (i < element.getIntMinOccurs()
                && state.mMinFactor[state.mModelLevel] > 0) {
            throwException("'"
                    + element.getName().getLocalPart()
                    + "' must occur at least "
                    + element.getIntMinOccurs()
                    + " time(s).", null, state);
        }
        forwardToken(state, state.mSemanticLevel);
        return i > 0;
    }

    private static void forwardToken(ParsingState state, int semanticLevel)
            throws IOException, LexicalException {
        if (state.mToken.mTokenType == Token.VALUE
                && mDelimType2Level[state.mToken.mDelimType]
                                    >= semanticLevel) {
            state.mLexer.fillNextToken(state.mToken);
        }
    }
    
    private static void skipToNextSegment(ParsingState state)
            throws IOException, LexicalException, SAXException { 
        while(state.mLexer.fillNextToken(state.mToken)) {
            if (state.mToken.mTokenType == Token.EOF
                    || state.mToken.mTokenType == Token.SEG_NAME) {
                break;
            }
        }
        if (state.mToken.mTokenType == Token.SEG_NAME) {
            if (state.mToken.mDelimType == Token.DELIM_NOT_READ) {
                //segment name got cut by buffer boundary
                state.mStringBuffer.setLength(0);
                state.mStringBuffer.append(state.mToken.mChars,
                        state.mToken.mOffset, state.mToken.mCount);
                state.mLexer.fillNextToken(state.mToken);
                if (state.mToken.mTokenType != Token.SEG_NAME
                        || state.mToken.mDelimType != Token.FIELD_SEP) {
                    throwException("Failed to read segment name",
                            null, state);
                }
                state.mStringBuffer.append(state.mToken.mChars,
                        state.mToken.mOffset, state.mToken.mCount);
                state.mSegName = state.mStringBuffer.toString();
            } else {
                state.mSegName =
                    new String(state.mToken.mChars, state.mToken.mOffset,
                            state.mToken.mCount);
            }
        }
    }
    
    private static boolean parseWildcard(SchemaParticle wildcard,
            ParsingState state, int sequence)
            throws IOException, SAXException {
        int i;
        switch (state.mSemanticLevel) {
        case DOCUMENT:
            i = 0;
            while (state.mToken.mTokenType == Token.SEG_NAME) {
                if (state.mPendingStartTagAt >= 0) {
                    //fire the start element events that are pending
                    for (int j = state.mPendingStartTagAt;
                            j < state.mModelLevel; j++) {
                        state.mContentHandler.startElement(
                                state.mQNamePath[j].getNamespaceURI(),
                                state.mQNamePath[j].getLocalPart(),
                                state.mQNamePath[j].getLocalPart(),
                                mEmptyAttributes);
                    }
                    state.mPendingStartTagAt = -1;
                }
                SchemaGlobalElement[] element = new SchemaGlobalElement[1];
                QName qName =
                    crackOutWildcardElement(wildcard, state.mSegName,
                            state.mRootElement.getTypeSystem(), element);
                if (qName == null) {
                    throwException(
                            "Unable to construct a qualified name for: '"
                            + state.mSegName + "'", null, state);
                }
                state.mLexer.fillNextToken(state.mToken);
                state.mContentHandler.startElement(
                        qName.getNamespaceURI(),
                        qName.getLocalPart(),
                        qName.getLocalPart(),
                        mEmptyAttributes);
                state.mSemanticLevel++;
                if (element[0] != null) {
                    //found the element definition
                    parseParticle(element[0].getType().getContentModel(),
                            state, sequence);
                } else {
                    //Process all fields
                    processWildcardFields(state, sequence);
                }
                state.mSemanticLevel--;
                state.mContentHandler.endElement(
                        qName.getNamespaceURI(),
                        qName.getLocalPart(),
                        qName.getLocalPart());
                skipToNextSegment(state);
                i++;
            }
            if (i < wildcard.getIntMinOccurs()
                    && state.mMinFactor[state.mModelLevel] > 0) {
                throwException("Wildcard segment must occur at least "
                        + wildcard.getIntMinOccurs()
                        + " time(s).", null, state);
            }
            return i > 0;
        case FIELD:
        case COMPONENT:
        case SUBCOMPONENT:
            return processWildcardFields(state, sequence);
        default:
            throw new IllegalStateException(
                    "Illegal semantic level: " + state.mSemanticLevel);
        }
    }

    private static boolean processWildcardFields(
            ParsingState state, int sequence)
                throws IOException, SAXException {
        
        if (state.mToken.mTokenType != Token.VALUE
                || state.mToken.mIsConsumed) {
            return false;
        }
        
        QName[] pendingEndTag = new QName[MAX_SEMANTIC_LEVELS];
        Arrays.fill(pendingEndTag, null);
        int curLevel = state.mSemanticLevel;
        if (curLevel < FIELD) {
            curLevel = FIELD;
        }
        int seenLevel = -1;
        int fieldIndex = 0;
        int fieldRepetIndex = -1;
        int compoIndex = 0;
        int subCompoIndex = 0;
        switch (state.mSemanticLevel) {
        case SEGMENT:
            fieldIndex = 0;
            compoIndex = 0;
            subCompoIndex = 0;
            break;
        case FIELD:
            fieldIndex = sequence - 1;
            compoIndex = 0;
            subCompoIndex = 0;
            break;
        case COMPONENT:
            compoIndex = sequence - 1;
            subCompoIndex = 0;
            break;
        case SUBCOMPONENT:
            subCompoIndex = sequence - 1;
            break;
        }
        while(state.mToken.mTokenType == Token.VALUE) {
            boolean useBuffer = false;
            state.mStringBuffer.setLength(0);
            //For wildcard processing, we have to see the delimiter
            //since we don't know the structure.  It's not like
            //for known elements, we can always fire characters(...)
            //even without seeing the delimiter.
            while(state.mToken.mDelimType == Token.DELIM_NOT_READ
                    && state.mToken.mTokenType == Token.VALUE) {
                state.mStringBuffer.append(
                        state.mToken.mChars, state.mToken.mOffset,
                        state.mToken.mCount);
                state.mLexer.fillNextToken(state.mToken);
                useBuffer = true;
            }
            if (state.mToken.mTokenType != Token.VALUE) {
                break;
            }
            if (state.mToken.mDelimType == Token.FIELD_SEP
                    && fieldRepetIndex == -1) {
                fieldRepetIndex = 0;
            }
            seenLevel = mDelimType2Level[state.mToken.mDelimType];
            if (curLevel <= seenLevel) {
                for (int j = curLevel; j <= seenLevel; j++) {
                    switch (j) {
                    case FIELD:
                        if (state.mToken.mDelimType == Token.REPET_SEP) {
                            if (fieldRepetIndex == -1) {
                                fieldRepetIndex = 1;
                            } else {
                                fieldRepetIndex++;
                            }
                        }
                        state.mStringBuffer2.setLength(0);
                        state.mStringBuffer2.append("fld");
                        state.mStringBuffer2.append(++fieldIndex);
                        if (fieldRepetIndex != -1) {
                            state.mStringBuffer2.append('_');
                            state.mStringBuffer2.append(
                                    ++fieldRepetIndex);
                        }
                        break;
                    case COMPONENT:
                        state.mStringBuffer2.setLength(0);
                        state.mStringBuffer2.append("com");
                        state.mStringBuffer2.append(++compoIndex);
                        break;
                    case SUBCOMPONENT:
                        state.mStringBuffer2.setLength(0);
                        state.mStringBuffer2.append("sub");
                        state.mStringBuffer2.append(++subCompoIndex);
                        break;
                    case SEGMENT:
                        //not possible
                    default:
                        throw new IllegalStateException(
                                "Illegal semantic level: " + j);
                    }
                    pendingEndTag[j] =
                        new QName(NS_URI_FOR_EXTENSIONS,
                                state.mStringBuffer2.toString());
                    state.mContentHandler.startElement(
                            pendingEndTag[j].getNamespaceURI(),
                            pendingEndTag[j].getLocalPart(),
                            pendingEndTag[j].getLocalPart(),
                            mEmptyAttributes);
                }
                fireCharacters(state, useBuffer);
                state.mToken.mIsConsumed = true;
                state.mContentHandler.endElement(
                        pendingEndTag[seenLevel].getNamespaceURI(),
                        pendingEndTag[seenLevel].getLocalPart(),
                        pendingEndTag[seenLevel].getLocalPart());
                pendingEndTag[seenLevel] = null;
            } else {
                switch (curLevel) {
                case FIELD:
                    if (state.mToken.mDelimType == Token.REPET_SEP) {
                        if (fieldRepetIndex == -1) {
                            fieldRepetIndex = 1;
                        } else {
                            fieldRepetIndex++;
                        }
                    }
                    state.mStringBuffer2.setLength(0);
                    state.mStringBuffer2.append("fld");
                    state.mStringBuffer2.append(++fieldIndex);
                    if (fieldRepetIndex != -1) {
                        state.mStringBuffer2.append('_');
                        state.mStringBuffer2.append(
                                ++fieldRepetIndex);
                    }
                    break;
                case COMPONENT:
                    state.mStringBuffer2.setLength(0);
                    state.mStringBuffer2.append("com");
                    state.mStringBuffer2.append(++compoIndex);
                    break;
                case SUBCOMPONENT:
                    state.mStringBuffer2.setLength(0);
                    state.mStringBuffer2.append("sub");
                    state.mStringBuffer2.append(++subCompoIndex);
                    break;
                case SEGMENT:
                    //not possible
                default:
                    throw new IllegalStateException(
                            "Illegal semantic level: " + curLevel);
                }
                if (pendingEndTag[curLevel] == null) {
                    pendingEndTag[curLevel] =
                        new QName(NS_URI_FOR_EXTENSIONS,
                                state.mStringBuffer2.toString());
                    state.mContentHandler.startElement(
                            pendingEndTag[curLevel].getNamespaceURI(),
                            pendingEndTag[curLevel].getLocalPart(),
                            pendingEndTag[curLevel].getLocalPart(),
                            mEmptyAttributes);
                }
                fireCharacters(state, useBuffer);
                state.mToken.mIsConsumed = true;
                state.mContentHandler.endElement(
                        pendingEndTag[curLevel].getNamespaceURI(),
                        pendingEndTag[curLevel].getLocalPart(),
                        pendingEndTag[curLevel].getLocalPart());
                pendingEndTag[curLevel] = null;
                for (int j = curLevel - 1; j >= seenLevel; j--) {
                    if (pendingEndTag[j] != null) {
                        state.mContentHandler.endElement(
                                pendingEndTag[j].getNamespaceURI(),
                                pendingEndTag[j].getLocalPart(),
                                pendingEndTag[j].getLocalPart());
                        pendingEndTag[j] = null;
                    }
                }
            }
            curLevel = seenLevel;
            if ((state.mToken.mDelimType
                    & mDelimEndMask[state.mSemanticLevel]) != 0) {
                break;
            }
            state.mLexer.fillNextToken(state.mToken);
        }
        if (curLevel < FIELD) {
            curLevel = FIELD;
        }
        forwardToken(state, curLevel);
        return true;
    }
    
    private static QName crackOutWildcardElement(SchemaParticle wildcard,
            String localName, SchemaTypeSystem ts,
            SchemaGlobalElement[] matchedElement) {
        QNameSet qnSet = wildcard.getWildcardSet();
        if (qnSet.containsAll(mQNameSetForExtensions)) {
            matchedElement[0] = null;
            return new QName(NS_URI_FOR_EXTENSIONS, localName);
        }
        if (qnSet.includedURIs() == null) {
            //don't know how to crack out
            return null;
        }
        QName qName;
        SchemaGlobalElement element = null;
        for (Iterator iter = qnSet.includedURIs().iterator();
                iter.hasNext();) {
            qName = new QName((String) iter.next(), localName);
            if ((element = ts.findElement(qName)) != null) {
                break;
            }
        }
        if (element != null) {
            matchedElement[0] = element;
            return element.getName();
        }
        //don't know how to crack out
        return null;
    }
    
    private static int whatKind(SchemaType XMLType) {
        if (XMLType.isSimpleType()
                || XMLType.getContentType() == SchemaType.SIMPLE_CONTENT) {
            return KIND_SIMPLE;
        }
        if (XMLType.getContentType() == SchemaType.MIXED_CONTENT) {
            return KIND_ESCAPE;
        }
        if (XMLType.isURType()) {
            return KIND_VARIES;
        }
        return KIND_COMPLEX;
    }

    private static void fireCharacters(ParsingState state, boolean useBuffer)
            throws SAXException {
        SwiftCharEscapeCoder coder = state.mLexer.mEscapeCoder;
        if (useBuffer) {
            if (state.mChars == null
                    || state.mChars.length
                        < state.mStringBuffer.length()) {
                state.mChars = new char[state.mStringBuffer.length()];
            }
            state.mStringBuffer.getChars(
                    0, state.mStringBuffer.length(), state.mChars, 0);
            if (coder != null && state.mToken.mHasEscape) {
                coder.unescape(state.mChars, 0,
                        state.mStringBuffer.length(), true);
                fireCoderResult(coder, state);
            } else {
                state.mContentHandler.characters(
                        state.mChars,
                        0,
                        state.mStringBuffer.length());
            }
        } else {
            if (coder != null && state.mToken.mHasEscape) {
                coder.unescape(
                        state.mToken.mChars,
                        state.mToken.mOffset,
                        state.mToken.mCount,
                        state.mToken.mDelimType
                            != Token.DELIM_NOT_READ);
                fireCoderResult(coder, state);
            } else {
                state.mContentHandler.characters(
                        state.mToken.mChars,
                        state.mToken.mOffset,
                        state.mToken.mCount);
            }
        }
    }
    
    private static void fireCoderResult(SwiftCharEscapeCoder coder,
            ParsingState state) throws SAXException {
        Result res;
        while (coder.hasResult()) {
            res = coder.getResult();
            switch (res.mType) {
            case Result.CHARDATA:
                state.mContentHandler.characters(
                        res.mData,
                        res.mStart,
                        res.mCount);
                break;
            case Result.ESCAPE:
                if (state.mEspAttr == null) {
                    state.mEspAttr = new EscapeAttributes();
                }
                state.mEspAttr.setValue(0,
                        new String(res.mData, res.mStart, res.mCount));
                state.mContentHandler.startPrefixMapping("", "");
                state.mContentHandler.startElement(
                        "",
                        "escape",
                        "escape",
                        state.mEspAttr);
                state.mContentHandler.endElement("", "escape", "escape");        
                state.mContentHandler.endPrefixMapping("");
                break;
            }
        }
    }
    
    private static void throwException(String msg, Exception cause,
            ParsingState state) throws SAXException {
        throw new SAXParseException(msg, new LocatorImpl(state), cause);
    }
            
    private static class ParsingState {
        public final SchemaGlobalElement mRootElement;
        public final Token mToken;
        public final Lexer mLexer;
        public final ContentHandler mContentHandler;
        public final StringBuffer mStringBuffer = new StringBuffer();
        public final StringBuffer mStringBuffer2 = new StringBuffer();
        public final String mGroupPrefix;
        public int mModelLevel;
        public QName[] mQNamePath = new QName[MAX_MODEL_LEVELS];
        public int mSemanticLevel;
        public int[] mMinFactor = new int[MAX_MODEL_LEVELS];
        public int mPendingStartTagAt;
        public String mPublicId;
        public String mSystemId;
        public String mSegName;
        public char[] mChars;
        public EscapeAttributes mEspAttr;
        
        public ParsingState(SchemaGlobalElement root, ContentHandler handler,
                Lexer lexer, String groupPrefix) {
            mRootElement = root;
            mToken = new Token();
            mContentHandler = handler;
            mLexer = lexer;
            mGroupPrefix = groupPrefix;
        }
    }
    
    private static class LocatorImpl implements Locator {

        private final ParsingState mState;
        
        public LocatorImpl(ParsingState state) {
            mState = state;
        }
        
        public String getPublicId() {
            return mState.mPublicId;
        }

        public String getSystemId() {
            return mState.mSystemId;
        }

        public int getLineNumber() {
            return mState.mToken != null ? mState.mToken.mLine : -1;
        }

        public int getColumnNumber() {
            return mState.mToken != null ? mState.mToken.mCol : -1;
        }
    }
    
    private static final class EscapeAttributes implements Attributes {
        
        private String mValue;

        public int getLength() {
            return 1;
        }

        public String getURI(int index) {
            if (index == 0) {
                return "";
            }
            return null;
        }

        public String getLocalName(int index) {
            if (index == 0) {
                return "V";
            }
            return null;
        }

        public String getQName(int index) {
            if (index == 0) {
                return "V";
            }
            return null;
        }

        public String getType(int index) {
            if (index == 0) {
                return "CDATA";
            }
            return null;
        }

        public String getValue(int index) {
            if (index == 0) {
                return mValue;
            }
            return null;
        }

        public int getIndex(String uri, String localName) {
            if (uri == null && "V".equals(localName)) {
                return 0;
            }
            return -1;
        }

        public int getIndex(String qName) {
            if ("V".equals(qName)) {
                return 0;
            }
            return -1;
        }

        public String getType(String uri, String localName) {
            if (uri == null && "V".equals(localName)) {
                return "CDATA";
            }
            return null;
        }

        public String getType(String qName) {
            if ("V".equals(qName)) {
                return "CDATA";
            }
            return null;
        }

        public String getValue(String uri, String localName) {
            if (uri == null && "V".equals(localName)) {
                return mValue;
            }
            return null;
        }

        public String getValue(String qName) {
            if ("V".equals(qName)) {
                return mValue;
            }
            return null;
        }
        
        public void setValue(int index, String value) {
            if (index == 0) {
                mValue = value;
            }
        }
    }
    
    private static final class EmptyAttributes implements Attributes {

        public int getLength() {
            return 0;
        }

        public String getURI(int index) {
            return null;
        }

        public String getLocalName(int index) {
            return null;
        }

        public String getQName(int index) {
            return null;
        }

        public String getType(int index) {
            return null;
        }

        public String getValue(int index) {
            return null;
        }

        public int getIndex(String uri, String localName) {
            return -1;
        }

        public int getIndex(String qName) {
            return -1;
        }

        public String getType(String uri, String localName) {
            return null;
        }

        public String getType(String qName) {
            return null;
        }

        public String getValue(String uri, String localName) {
            return null;
        }

        public String getValue(String qName) {
            return null;
        }
    }
    
    public static final class Token {
        //token types
        public static final int SEG_NAME = 1;
        public static final int VALUE = 2;
        public static final int EOF = 3;
        
        //delimiter types.  !!!**** Attention: if any of the following
        //values get changed or new values are added, please also modify the
        //mDelimType2Level array in UnmarshalAdaptor class.  Also, please
        //do NOT change the order of these values and make sure they are
        //exponents of 2.
        public static final int DELIM_NOT_READ = 1;
        public static final int SEG_TERM = 2;
        public static final int FIELD_SEP = 4;
        public static final int REPET_SEP = 8;
        public static final int COMPO_SEP = 16;
        public static final int SUBCOMPO_SEP = 32;
        //Escape char is not a delimiter, but we need this value for
        //performance optimization
        public static final int ESCAPE_CHAR = 64;
        
        public int mTokenType;
        public char[] mChars;
        public int mOffset;
        public int mCount;
        public int mDelimType;
        public int mLine;
        public int mCol;
        public int mLexerPos;
        public boolean mToBeContinued;
        public boolean mHasEscape;
        public boolean mIsConsumed;
    }

    static final class Lexer {
        
        private static final int MSH_FIELD_SEP = 1;
        private static final int MSH_ENCODING_CHAR = 2;
        
        //states
        private static final int NOT_STARTED = 0;
        private static final int SEG_END = 1;
        private static final int SEG_NAME_READ = 2;
        private static final int SEG_NAME_READ_EOF = 3;
        private static final int EOF = 4;
        
        //segment types
        private static final int MSH_SEG = 0;
        private static final int NORM_SEG = 1;
        
        //special delimiter states
        private static final int DELIM_UNKNOWN = -1;
        private static final int MULTIPLE_HIT = -2;

        //segment name size
        private static final int SEG_NAME_SIZE = 3;
        
        private final Reader mReader;
        private final int mBufCapacity;
        private final char[] mRawCharBuf;
        
        public SwiftCharEscapeCoder mEscapeCoder;
        
        private char mSegDelim = '\r';
        private char mFieldSep = '|';
        private char mCompoSep = '^';
        private char mRepetSep = '~';
        private char mEscape = '\\';
        private char mSubCompoSep = '&';
        private int[] mDelimChar = new int[256];
        
        private int mState = NOT_STARTED;
        private int mFieldSequence = -1;
        private int mSegType = -1;
        private int mRawPos = 0;
        private int mBufLimit = -1;
        private int mLine = 1;
        private int mCol = 1;
        private boolean mLoadBuf;
        
        public Lexer(InputSource input) throws IOException {
            this(input, DEFAULT_BUF_CAPACITY);
        }
        
        public Lexer(InputSource input, int bufCapacity) throws IOException {
            if (bufCapacity < MINIMUM_BUF_CAPACITY
                    || bufCapacity > MAXIMUM_BUF_CAPACITY) {
                throw new IllegalArgumentException(
                        "Buffer capacity must be in range of ["
                        + MINIMUM_BUF_CAPACITY + ", "
                        + MAXIMUM_BUF_CAPACITY + "] bytes.");
            }
            mBufCapacity = bufCapacity;
            mRawCharBuf = new char[mBufCapacity];
            if (input.getCharacterStream() == null) {
                InputStream stream = input.getByteStream();
                if (stream == null) {
                    throw new IllegalArgumentException(
                        "No reader and no input stream.");
                }
                if (input.getEncoding() != null) {
                    mReader =
                        new InputStreamReader(stream, input.getEncoding());
                } else {
                    mReader = new InputStreamReader(stream, "ASCII");
                }
            } else {
                mReader = input.getCharacterStream();
            }
            Arrays.fill(mDelimChar, 0);
            mBufLimit = mReader.read(mRawCharBuf);
            if (mBufLimit == -1) {
                mState = EOF;
            }
            mLoadBuf = false;
        }
        
        public final boolean fillNextToken(Token token)
                throws IOException, LexicalException {
            
            token.mIsConsumed = false;
            token.mHasEscape = false;
            if (mState == EOF) {
                token.mTokenType = Token.EOF;
                token.mDelimType = Token.SEG_TERM;
                return false;
            }
            if (mRawPos > mBufLimit) {
                //illegal
                throw new IllegalStateException(
                        "mRawPos (" + mRawPos
                        + ") cannot be greater than mBufLimit ("
                        + mBufLimit);
            }
            if (!mLoadBuf && mRawPos == mBufLimit) {
                mLoadBuf = true;
            }
            if (mLoadBuf) {
                mLoadBuf = false;
                //reach buffer limit, load data
                int remain = mBufLimit - mRawPos;
                if (remain != 0) {
                    System.arraycopy(mRawCharBuf, mRawPos,
                            mRawCharBuf, 0, remain);
                    mBufLimit = mReader.read(mRawCharBuf, remain,
                            mBufCapacity - remain);
                    if (mBufLimit == -1) {
                        mState = SEG_NAME_READ_EOF;
                        mBufLimit = remain;
                    } else {
                        mBufLimit += remain;
                    }
                } else {
                    mBufLimit = mReader.read(mRawCharBuf);
                    if (mBufLimit == -1) {
                        mState = EOF;
                        token.mTokenType = Token.EOF;
                        token.mDelimType = Token.SEG_TERM;
                        return false;
                    }
                }
                mRawPos = 0;
            }
            
            int nextRawPos;
            int nextCol;
            int delimType;
            char c;
            int ci;
            int i;
            switch (mState) {
            case SEG_NAME_READ:
            case SEG_NAME_READ_EOF:
                if (mSegType == MSH_SEG) {
                    switch (mFieldSequence) {
                    case MSH_FIELD_SEP:
                        //field separator
                        mFieldSep = mRawCharBuf[mRawPos];
                        token.mChars = mRawCharBuf;
                        token.mOffset = mRawPos;
                        token.mCount = 1;
                        token.mDelimType = Token.FIELD_SEP;
                        token.mLexerPos = mRawPos;
                        token.mTokenType = Token.VALUE;
                        token.mLine = mLine;
                        token.mCol = mCol;
                        token.mToBeContinued = false;
                        mFieldSequence++;
                        mRawPos++;
                        mCol++;
                        return true;
                    case MSH_ENCODING_CHAR:
                        //encoding characters
                        nextRawPos = mRawPos;
                        nextCol = mCol;
                        i = 0;
                        while (nextRawPos < mBufLimit) {
                            c = mRawCharBuf[nextRawPos];
                            if (c == mFieldSep) {
                                break;
                            }
                            switch (i) {
                            case 0:
                                mCompoSep = c;
                                break;
                            case 1:
                                mRepetSep = c;
                                break;
                            case 2:
                                mEscape = c;
                                break;
                            case 3:
                                mSubCompoSep = c;
                                break;
                            }
                            nextRawPos++;
                            i++;
                            nextCol++;
                        }
                        if (nextRawPos < mBufLimit) {
                            //stopped by field separator, good
                            mDelimChar[mSegDelim & 0xFF] = Token.SEG_TERM;
                            if (mDelimChar[mFieldSep & 0xFF] != 0) {
                                //conflict
                                mDelimChar[mFieldSep & 0xFF] = MULTIPLE_HIT;
                            } else {
                                mDelimChar[mFieldSep & 0xFF] = Token.FIELD_SEP;
                            }
                            if (mDelimChar[mCompoSep & 0xFF] != 0) {
                                //conflict
                                mDelimChar[mCompoSep & 0xFF] = MULTIPLE_HIT;
                            } else {
                                mDelimChar[mCompoSep & 0xFF] = Token.COMPO_SEP;
                            }
                            if (mDelimChar[mRepetSep & 0xFF] != 0) {
                                //conflict
                                mDelimChar[mRepetSep & 0xFF] = MULTIPLE_HIT;
                            } else {
                                mDelimChar[mRepetSep & 0xFF] = Token.REPET_SEP;
                            }
                            if (mDelimChar[mSubCompoSep & 0xFF] != 0) {
                                //conflict
                                mDelimChar[mSubCompoSep & 0xFF] = MULTIPLE_HIT;
                            } else {
                                mDelimChar[mSubCompoSep & 0xFF] =
                                    Token.SUBCOMPO_SEP;
                            }
                            if (mDelimChar[mEscape & 0xFF] != 0) {
                                //conflict
                                mDelimChar[mEscape & 0xFF] = MULTIPLE_HIT;
                            } else {
                                mDelimChar[mEscape & 0xFF] =
                                    Token.ESCAPE_CHAR;
                            }
                            mEscapeCoder =
                                new SwiftCharEscapeCoder(mFieldSep, mCompoSep,
                                        mSubCompoSep, mRepetSep, mEscape);
                            token.mChars = mRawCharBuf;
                            token.mOffset = mRawPos;
                            token.mCount = i;
                            token.mDelimType = Token.FIELD_SEP;
                            token.mLexerPos = mRawPos;
                            token.mTokenType = Token.VALUE;
                            token.mLine = mLine;
                            token.mCol = mCol;
                            token.mToBeContinued = false;
                            mRawPos = nextRawPos + 1;
                            mCol = nextCol + 1;
                            mFieldSequence++;
                            return true;
                        }
                        throwLexException("Missing field separator.", mLine,
                                mCol + 4, 1, mRawPos + 4);
                    }
                }
                //either not a MSH segment or fields in MSH segment that
                //do not require special handling
                nextRawPos = mRawPos;
                nextCol = mCol;
                delimType = DELIM_UNKNOWN;
                whileloop: while (nextRawPos < mBufLimit) {
                    ci = mRawCharBuf[nextRawPos] & 0xFF;
                    if (mDelimChar[ci] != 0) {
                        //might have found delimiter, check further
                        delimType = mDelimChar[ci]; 
                        switch (delimType) {
                        case Token.FIELD_SEP:
                            if (mRawCharBuf[nextRawPos] == mFieldSep) {
                                break whileloop;
                            }
                            //no actual match
                            break;
                        case Token.COMPO_SEP:
                            if (mRawCharBuf[nextRawPos] == mCompoSep) {
                                break whileloop;
                            }
                            //no actual match
                            break;
                        case Token.REPET_SEP:
                            if (mRawCharBuf[nextRawPos] == mRepetSep) {
                                break whileloop;
                            }
                            //no actual match
                            break;
                        case Token.SUBCOMPO_SEP:
                            if (mRawCharBuf[nextRawPos]
                                            == mSubCompoSep) {
                                break whileloop;
                            }
                            //no actual match
                            break;
                        case Token.SEG_TERM:
                            if (mRawCharBuf[nextRawPos]
                                            == mSegDelim) {
                                break whileloop;
                            }
                            //no actual match
                            break;
                        case Token.ESCAPE_CHAR:
                            if (mRawCharBuf[nextRawPos]
                                            == mEscape) {
                                token.mHasEscape = true;
                            }
                            nextRawPos++;
                            nextCol++;
                            continue whileloop;
                        case MULTIPLE_HIT:
                            //conflict hash code, check which one it is.
                            //should rarely happen
                            c = mRawCharBuf[nextRawPos];
                            if (c == mFieldSep) {
                                delimType = Token.FIELD_SEP;
                                break whileloop;
                            } else if(c == mCompoSep) {
                                delimType = Token.COMPO_SEP;
                                break whileloop;
                            } else if(c == mRepetSep) {
                                delimType = Token.REPET_SEP;
                                break whileloop;
                            } else if(c == mSubCompoSep) {
                                delimType = Token.SUBCOMPO_SEP;
                                break whileloop;
                            } else if(c == mSegDelim) {
                                delimType = Token.SEG_TERM;
                                break whileloop;
                            }
                            //no actual match
                            break;
                        }
                    }
                    nextRawPos++;
                    nextCol++;
                }
                if (nextRawPos < mBufLimit) {
                    //stopped by a delimiter
                    assert delimType != DELIM_UNKNOWN &&
                        delimType != MULTIPLE_HIT
                        && delimType != Token.DELIM_NOT_READ;
                    token.mChars = mRawCharBuf;
                    token.mOffset = mRawPos;
                    token.mCount = nextRawPos - mRawPos;
                    token.mDelimType = delimType;
                    token.mLexerPos = mRawPos;
                    token.mTokenType = Token.VALUE;
                    token.mLine = mLine;
                    token.mCol = mCol;
                    token.mToBeContinued = false;
                    mRawPos = nextRawPos + 1;
                    if (delimType == Token.SEG_TERM) {
                        mLine++;
                        mCol = 1;
                        mFieldSequence = 1;
                        mState = SEG_END;
                    } else {
                        if (delimType == Token.FIELD_SEP) {
                            mFieldSequence++;
                        }
                        mCol = nextCol + 1;
                    }
                } else {
                    token.mChars = mRawCharBuf;
                    token.mOffset = mRawPos;
                    token.mCount = nextRawPos - mRawPos;
                    if (mState == SEG_NAME_READ_EOF) {
                        token.mDelimType = Token.SEG_TERM;
                        mState = EOF;
                    } else {
                        token.mDelimType = Token.DELIM_NOT_READ;
                    }
                    token.mLexerPos = mRawPos;
                    token.mTokenType = Token.VALUE;
                    token.mLine = mLine;
                    token.mCol = mCol;
                    token.mToBeContinued = true;
                    mRawPos = nextRawPos;
                    mCol = nextCol;
                }
                return true;
            case SEG_END:
                nextRawPos = mRawPos;
                nextCol = mCol;
                i = 0;
                while (nextRawPos < mBufLimit && i <= SEG_NAME_SIZE) {
                    c = mRawCharBuf[nextRawPos];
                    if (c == mFieldSep) {
                        break;
                    }
                    nextRawPos++;
                    i++;
                    nextCol++;
                }
                if (i > SEG_NAME_SIZE) {
                    throwLexException("Segment name cannot be more"
                            + " than 3 characters.", mLine,
                            mCol, 4, mRawPos);
                }
                token.mChars = mRawCharBuf;
                token.mOffset = mRawPos;
                token.mCount = i;
                token.mLexerPos = mRawPos;
                token.mTokenType = Token.SEG_NAME;
                token.mLine = mLine;
                token.mCol = mCol;
                if (nextRawPos < mBufLimit) {
                    token.mDelimType = Token.FIELD_SEP;
                    token.mToBeContinued = false;
                    mState = SEG_NAME_READ;
                    mSegType = NORM_SEG;
                    mRawPos = nextRawPos + 1;
                    mCol = nextCol + 1;
                } else {
                    mLoadBuf = true;
                    token.mDelimType = Token.DELIM_NOT_READ;
                    token.mToBeContinued = true;
                    mRawPos = nextRawPos;
                    mCol = nextCol;
                }
                return true;
            case NOT_STARTED:
                nextRawPos = mRawPos + SEG_NAME_SIZE;
                if (nextRawPos > mBufLimit) {
                    throwLexException(
                            "Incomplete segment name: '"
                                + new String(mRawCharBuf, 0, mBufLimit) + "'",
                            mLine, mCol, mBufLimit, mBufLimit);
                }
                token.mChars = mRawCharBuf;
                token.mOffset = 0;
                token.mCount = SEG_NAME_SIZE;
                token.mDelimType = Token.FIELD_SEP;
                token.mLexerPos = 0;
                token.mTokenType = Token.SEG_NAME;
                token.mLine = mLine;
                token.mCol = mCol;
                token.mToBeContinued = false;
                mFieldSequence = 1;
                //TODO: Might be FHS or BHS, but right now we only
                //handles MSH as the only starting segment. 
                mSegType = MSH_SEG;
                mState = SEG_NAME_READ;
                mRawPos += SEG_NAME_SIZE;
                mCol += SEG_NAME_SIZE;
                return true;
            default:
                //Not possible unless programming error
                throw new IllegalStateException(
                        "Illegal State = " + mState);
            }
        }
        
        private void throwLexException(String msg, int line, int col,
                int count, int pos) throws LexicalException {
            throw new LexicalException(msg, line, col, count);
        }
    }
}
