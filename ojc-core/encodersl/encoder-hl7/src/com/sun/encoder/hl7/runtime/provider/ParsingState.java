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

import org.apache.xmlbeans.SchemaGlobalElement;
import org.xml.sax.ContentHandler;
import javax.xml.namespace.QName;

class ParsingState {

    private static final int MAX_MODEL_LEVELS = 50;

    public final SchemaGlobalElement mRootElement;
    public final Token mToken;
    public final Lexer mLexer;
    public final ContentHandler mContentHandler;

    /**
     * used anytime a string buffer is needed
     */
    public final StringBuffer mSB = new StringBuffer();
    /**
     * only used in parsing wildcard elements (represented as xsd:any's), such
     * as Z segments, or extra trailing fields, components etc.
     */
    public final StringBuffer mSB2 = new StringBuffer();

    public final String mGroupPrefix;
    public int mModelLevel;
    public QName[] mQNamePath = new QName[MAX_MODEL_LEVELS];

    /**
     * semantic level.
     * @see SemLevel#DOCUMENT
     * @see SemLevel#SEGMENT
     * @see SemLevel#FIELD
     * @see SemLevel#COMPONENT
     * @see SemLevel#SUBCOMPONENT
     */
    public int mSemanticLevel;

    /**
     * This stands for minimum occurrence factor.  The usage is to indicate
     * the effective minimum occurrence passed down from ascendants of the
     * current node.  For example, considering following structure:
     * <pre>
     * A-->(B?, C)
     * B-->(X, Y)
     * </pre>
     * If the input is C, when the parser is processing node X and seeing input
     * C, it should not fail although node X is mandatory. Basically through
     * minFactor the parser knows that the effective minimum occurrence for node
     * X is 0, so it will not fail on node X when it sees input C.  But if input
     * is X, C, then the parser should fail on node Y when it sees C.  Once the
     * first node of the current semantic level (here, it is node X) is seen,
     * the effect minimum occurrence of the level becomes 1, which means, the
     * passed down effective minimum occurrence will be ignored (the reason is
     * that the currrent level is already presented, there is no need to use
     * ascendant's optionality to determine whether the nodes in current level
     * should occur or not).
     */
    public int[] mMinFactor = new int[MAX_MODEL_LEVELS];
    // pending start tag at which model level
    public int mModelLevelPendingStartTagAt;
    public String mPublicId;
    public String mSystemId;
    public String mSegName;
    public char[] mChars;

    public EscapeAttributes mEspAttr;

    public ParsingState(SchemaGlobalElement root, ContentHandler handler,
        Lexer lexer, String groupPrefix) {
        mRootElement = root;
        // creates a new empty token
        mToken = new Token();
        mContentHandler = handler;
        mLexer = lexer;
        mGroupPrefix = groupPrefix;
    }

    @Override
    public String toString() {
        StringBuffer buff = new StringBuffer("ParsingState@")
            .append(Integer.toHexString(hashCode()));
        // buff.append("\nrootElement='").append(mRootElement.getName()).append("'");
        // buff.append(" groupPrefix='").append(mGroupPrefix).append("'");
        buff.append("\nlexer=").append(mLexer);
        buff.append("\ntoken=").append(mToken);
        buff.append("\nmodelLevel=").append(mModelLevel);
        buff.append(" semanticLevel=")
            .append(SemLevel.getSemanticLevelDesc(mSemanticLevel));
        if (mSegName != null) {
            buff.append(" segName='").append(mSegName).append("'");
        }
        if (mChars != null) {
            buff.append("\nchars=").append(mChars);
        }
        buff.append("\nminFactor=[");
        for (int i = 0; i < MAX_MODEL_LEVELS; i++) {
            if (mMinFactor[i] > 0) {
                buff.append(i).append(":").append(mMinFactor[i]).append(" ");
            }
        }
        buff.append("]");
        return buff.toString();
    }
}
