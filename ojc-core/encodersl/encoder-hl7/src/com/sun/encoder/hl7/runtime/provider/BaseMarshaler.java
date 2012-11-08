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

package com.sun.encoder.hl7.runtime.provider;
import com.sun.encoder.hl7.i18n.Messages;
import java.util.HashSet;
import java.util.Set;

import com.sun.encoder.hl7.runtime.provider.MatchResult;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaProperty;
import org.apache.xmlbeans.SchemaType;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
abstract class BaseMarshaler implements Marshaler {

    static Messages mMessages =
        Messages.getMessages(BaseMarshaler.class);

    protected SchemaParticle mModel;
    protected int mChildrenCount;
    protected SchemaType mXMLType;
    // child index starting from 0
    protected int mChildIndex;
    protected int mRepeatIndex;
    private Set<QName> mMatchedParts;
    private boolean mHasMeaninglessGroup;

    public BaseMarshaler(SchemaType xmlType) {
        reset(xmlType);
    }

    public void reset(SchemaType xmlType) {
        mXMLType = xmlType;
        mModel = xmlType.getContentModel();
        if (mModel.getParticleType() == SchemaParticle.ELEMENT
            || mModel.getParticleType() == SchemaParticle.WILDCARD) {
            // Must be the case that a meaningless/pointless group is removed
            mHasMeaninglessGroup = true;
            mChildrenCount = 1;
        } else {
            mHasMeaninglessGroup = false;
            mChildrenCount = mModel.countOfParticleChild();
        }
        mChildIndex = -1;
        mRepeatIndex = -1;
        mMatchedParts = null;
    }

    protected abstract void endOfRepetition();

    /**
     * Try to locate the metadata from the XML Schema model based on input
     * uri and localName. The matched result is in the MatchResult object,
     * which is populated with the matched element and its type details.
     * @param uri
     * @param localName
     * @param result - to be populated with the matched element and its type
     * details once matched.
     * @throws org.xml.sax.SAXException
     */
    protected final void match(String uri, String localName,
        MatchResult result) throws SAXException {
        switch (mModel.getParticleType()) {
            case SchemaParticle.SEQUENCE:
            // In case of meaningless group, element or wildcard particle is
            // also possible
            case SchemaParticle.ELEMENT:
            case SchemaParticle.WILDCARD:
                sequenceMatch(uri, localName, result);
                break;
            case SchemaParticle.CHOICE:
                choiceMatch(uri, localName, result);
                break;
            case SchemaParticle.ALL:
                allMatch(uri, localName, result);
                break;
            default:
                String errMsg = mMessages.getString("HL7ENC-E0008.Illegal_Particle_Type",
                    new Object[]{mModel.getParticleType()});
                // Even though particle can also be wildcard or element, but
                // according to the logic, only model group particles are
                // accepted here
                throw new SAXException(errMsg);
        }
    }

    protected final void endMatch(String uri, String localName)
        throws SAXException {
        String errMsg = null;
        switch (mModel.getParticleType()) {
            case SchemaParticle.SEQUENCE:
            // In case of meaningless group, element or wildcard particle is
            // also possible
            case SchemaParticle.ELEMENT:
            case SchemaParticle.WILDCARD:
                if (mChildrenCount == 0 || mChildIndex >= mChildrenCount) {
                    return;
                }
                int repet = mRepeatIndex;
                for (int i = mChildIndex; i < mChildrenCount; i++) {
                    if (repet <
                        mModel.getParticleChild(i).getIntMinOccurs() - 1) {
                        errMsg = mMessages.getString("HL7ENC-E0015.Unexpected_Element",
                            new Object[]{mModel.getParticleChild(i).getName(), uri, localName});
                        throw new SAXException(errMsg);
                    }
                    repet = -1;
                }
                break;
            case SchemaParticle.CHOICE:
                for (int i = 0; i < mChildrenCount; i++) {
                    if (mModel.getParticleChild(i).getIntMinOccurs() == 0) {
                        return;
                    }
                }
                if (mChildIndex == -1) {
                    errMsg = mMessages.getString("HL7ENC-E0016.Choice_group_with_no_child",
                        new Object[]{uri, localName});
                    throw new SAXException(errMsg);
                }
                break;
            case SchemaParticle.ALL:
                for (int i = 0; i < mChildrenCount; i++) {
                    if (mModel.getParticleChild(i).getIntMinOccurs() > 0
                        && !mMatchedParts.contains(mModel.getParticleChild(i).getName())) {
                        errMsg = mMessages.getString("HL7ENC-E0015.Unexpected_Element",
                            new Object[]{uri, localName});
                        throw new SAXException(errMsg);
                    }
                }
                break;
            default:
                errMsg = mMessages.getString("HL7ENC-E0008.Illegal_Particle_Type",
                    new Object[]{mModel.getParticleType()});
                // Even though particle can also be wildcard or element, but
                // according to the logic, only model group particles are
                // accepted here
                throw new SAXException(errMsg);
        }
    }

    private final void sequenceMatch(String uri, String localName,
        MatchResult result) throws SAXException {
        String errMsg = null;
        if (mChildIndex == -1) {
            // have not yet matched even first child
            mChildIndex = 0; // try first child
            mRepeatIndex = -1; // for occurrence
        }
        if (mChildIndex >= mChildrenCount) {
            errMsg = mMessages.getString("HL7ENC-E0017.Exceeding_end_of_seq_group",
                new Object[]{uri, localName});
            throw new SAXException(errMsg);
        }
        SchemaParticle childPart;
        if (mHasMeaninglessGroup) {
            childPart = mModel;
        } else {
            childPart = mModel.getParticleChild(mChildIndex);
        }
        boolean isWildcard =
            (childPart.getParticleType() == SchemaParticle.WILDCARD);
        boolean isMatched = false;
        if (isWildcard) {
            isMatched = isMatchedWildcard(uri, localName, childPart);
        } else {
            isMatched = isMatchedExactly(uri, localName, childPart);
        }
        if (!isMatched) {
            // i.e. not a repeating instance of previous child element
            if (mRepeatIndex > 0) {
                endOfRepetition();
            }
            mRepeatIndex = -1;
            mChildIndex++; // move on to next child element
            while (mChildIndex < mChildrenCount) {
                // try to match each of leftover children one by one
                childPart = mModel.getParticleChild(mChildIndex);
                isWildcard =
                    (childPart.getParticleType() == SchemaParticle.WILDCARD);
                if (isWildcard) {
                    isMatched = isMatchedWildcard(uri, localName, childPart);
                } else {
                    isMatched = isMatchedExactly(uri, localName, childPart);
                }
                if (isMatched) {
                    mRepeatIndex = 0;
                    break; // break-- while (mChildIndex < mChildrenCount)
                }
                if (childPart.getIntMinOccurs() > 0) {
                    if (!isWildcard) {
                        errMsg = mMessages.getString("HL7ENC-E0018.Unexpected_Element",
                            new Object[]{childPart.getName(), uri, localName});
                        throw new SAXException(errMsg);
                    } else {
                        errMsg = mMessages.getString("HL7ENC-E0019.Unexpected_Element",
                            new Object[]{childPart.getWildcardSet(), uri, localName});
                        throw new SAXException(errMsg);
                    }
                }
                mChildIndex++;
                mRepeatIndex = -1;
            } // end-- while (mChildIndex < mChildrenCount)
            if (!isMatched) {
                errMsg = mMessages.getString("HL7ENC-E0020.No_matching_particle_found",
                    new Object[]{uri, localName});
                throw new SAXException(errMsg);
            }
        } else {
            ++mRepeatIndex; // increase occurrence
            if (childPart.getMaxOccurs() != null
                && mRepeatIndex >= childPart.getIntMaxOccurs()) {
                errMsg = mMessages.getString("HL7ENC-E0021.Too_many_elements",
                    new Object[]{uri, localName});
                throw new SAXException(errMsg);
            }
        }
        // In HL7, the direct child particle of a model group is
        // always either an element or a wildcard. Popualte MatchResult object.
        if (childPart.getParticleType() == SchemaParticle.WILDCARD) {
            result.mResult = MatchResult.MATCH_WILDCARD;
            result.mElementName = new QName(uri, localName);
            result.mElementType = null;
        } else {
            // must be an element
            result.mResult = MatchResult.MATCH_ELEMENT;
            result.mElementName = childPart.getName();
            result.mElementType = childPart.getType();
        }
    }

    private final void choiceMatch(String uri, String localName,
        MatchResult result) throws SAXException {
        String errMsg = null;
        boolean matchedWildcard = false;
        boolean matchedElement = false;
        SchemaParticle childPart = null;
        if (mChildIndex >= 0) {
            childPart = mModel.getParticleChild(mChildIndex);
            if (childPart.getParticleType() == SchemaParticle.WILDCARD) {
                if (!isMatchedWildcard(uri, localName, childPart)) {
                    errMsg = mMessages.getString("HL7ENC-E0022.Element_does_not_match_wildcard",
                        new Object[]{uri, localName});
                    // previously matched a wildcard already, now an
                    // element that does not match the wildcardis coming.
                    // This is not allowed.
                    throw new SAXException(errMsg);
                } else {
                    matchedWildcard = true;
                }
            } else {
                if (!isMatchedExactly(uri, localName, childPart)) {
                    errMsg = mMessages.getString("HL7ENC-E0023.This_element_is_not_allowed_in_choice_group",
                        new Object[]{childPart.getName(), uri, localName});
                    // previously matched an element already, now a
                    // different element is coming. This is not allowed.
                    throw new SAXException(errMsg);
                } else {
                    matchedElement = true;
                }
            }
        }
        if (matchedWildcard) {
            mRepeatIndex++;
            if (childPart.getMaxOccurs() != null
                && mRepeatIndex >= childPart.getIntMaxOccurs()) {
                errMsg = mMessages.getString("HL7ENC-E0021.Too_many_elements",
                    new Object[]{uri, localName});
                throw new SAXException(errMsg);
            }
            result.mResult = MatchResult.MATCH_WILDCARD;
            result.mElementName = new QName(uri, localName);
            result.mElementType = null;
            return;
        }
        if (matchedElement) {
            mRepeatIndex++;
            if (childPart.getMaxOccurs() != null
                && mRepeatIndex >= childPart.getIntMaxOccurs()) {
                errMsg = mMessages.getString("HL7ENC-E0021.Too_many_elements",
                    new Object[]{uri, localName});
                throw new SAXException(errMsg);
            }
            result.mResult = MatchResult.MATCH_ELEMENT;
            result.mElementName = childPart.getName();
            result.mElementType = childPart.getType();
            return;
        }
        // Has never been matched before, match it.
        for (int i = 0; i < mChildrenCount; i++) {
            childPart = mModel.getParticleChild(i);
            if (childPart.getParticleType() == SchemaParticle.WILDCARD) {
                if (isMatchedWildcard(uri, localName, childPart)) {
                    matchedWildcard = true;
                    mChildIndex = i;
                    break;
                }
            } else if (isMatchedExactly(uri, localName, childPart)) {
                mChildIndex = i;
                matchedElement = true;
                break;
            }
        }
        if (matchedWildcard) {
            mRepeatIndex++;
            result.mResult = MatchResult.MATCH_WILDCARD;
            result.mElementName = new QName(uri, localName);
            result.mElementType = null;
            return;
        }
        if (matchedElement) {
            mRepeatIndex++;
            result.mResult = MatchResult.MATCH_ELEMENT;
            result.mElementName = childPart.getName();
            result.mElementType = childPart.getType();
            return;
        }
        errMsg = mMessages.getString("HL7ENC-E0020.No_matching_particle_found",
            new Object[]{uri, localName});
        throw new SAXException(errMsg);
    }

    private final void allMatch(String uri, String localName,
            MatchResult result) throws SAXException {
        String errMsg = null;
        if (mMatchedParts == null) {
            mMatchedParts = new HashSet<QName>();
        }
        QName name = new QName(uri, localName);
        // "all" model group can only have element declarations
        SchemaProperty elemProp = mXMLType.getElementProperty(name);
        if (elemProp == null) {
            errMsg = mMessages.getString("HL7ENC-E0020.No_matching_particle_found",
                new Object[]{uri, localName});
            throw new SAXException(errMsg);
        }
        if (mMatchedParts.contains(name)) {
            errMsg = mMessages.getString("HL7ENC-E0021.Too_many_elements",
                new Object[]{uri, localName});
            // "all" model group can only have child particles with
            // maxOccurs equal to one
            throw new SAXException(errMsg);
        }
        mMatchedParts.add(name);
        result.mResult = MatchResult.MATCH_ELEMENT;
        result.mElementName = elemProp.getName();
        result.mElementType = elemProp.getType();

        // we don't know what the child index is, but at least set
        // the repetition index
        mRepeatIndex = 0;
    }

    private boolean isMatchedWildcard(String uri, String localPart,
        SchemaParticle wildcard) {
        return wildcard.getWildcardSet().contains(new QName(uri, localPart));
    }

    private boolean isMatchedExactly(String uri, String localPart,
        SchemaParticle particle) {
        // Only checking the local name for the sake of performance
        return localPart.equals(particle.getName().getLocalPart());
    }
}

