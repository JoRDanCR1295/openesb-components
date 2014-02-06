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
 * @(#)DOMSorter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.xml.transform.sware.impl;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.xml.transform.sware.ElementNotFoundInComplexTypeException;
import com.sun.xml.transform.sware.InvalidSchemaException;
import com.sun.xml.transform.sware.MissingSchemaInfoException;
import com.sun.xml.transform.sware.OrderIndeterministicException;
import com.sun.xml.transform.sware.TooManyElementsException;
import com.sun.xml.transform.sware.SwareDOMImplementation.FOPolicy;
import com.sun.xml.transform.sware.impl.PureLinkedList.Entry;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType;
import com.sun.xml.transform.sware.schema.SwareGroup;
import com.sun.xml.transform.sware.schema.SwareParticle;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareType;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.AggregatedElemDecl;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.AggregatedParticle;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.AggregatedWildcard;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.LeafParticleType;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.ParticleAppearance;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType.ParticlePath;
import com.sun.xml.transform.sware.schema.SwareParticle.ParticleType;

/**
 * Sorts a DOM tree at one level. The algorithm looks up an aggregated particle
 * from a flattened complex type model group and then uses the particle path
 * stored in the aggregated particle to determine if an ordered insertion to the
 * sorted list is possible. A dynamic particle tree is maintained, which
 * reflects the static particle tree structure but adds another dimention, which
 * represents the axis for repeating nodes.
 *
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.9 $
 */
class DOMSorter {

    private final ReorderTransformerImpl mTransformer;
    private final SwareFlatComplexType mFlatComplexType;
    private final Element mSourceElement;
    private final Element mTargetElement;
    private final Document mTargetDocument;
    private final List<Tile> mUnsortedList = new ArrayList<Tile>();
    private final PureLinkedList<Tile> mSortedList = new PureLinkedList<Tile>();
    private final RootXNode mRootX;
    private boolean mInplace = true;

    /**
     * Constructs from a transformer, a flattened complex type, a source elemnt
     * and a target element.
     *
     * @param transformer the reordering transformer that provides context
     * information
     * @param fct the flattened complex type for easy lookup
     * @param srcElem the source element to be sorted on
     * @param tgtElem the target element that holds the sorted children
     * @throws SwareSchemaException invalid schema exception
     */
    DOMSorter(ReorderTransformerImpl transformer,
            SwareFlatComplexType fct, Element srcElem, Element tgtElem)
            throws SwareSchemaException {
        if (fct == null) {
            throw new NullPointerException("no flat complex type.");
        }
        if (srcElem == null) {
            throw new NullPointerException("no element.");
        }
        mFlatComplexType = fct;
        mSourceElement = srcElem;
        mTargetElement = tgtElem;
        if (tgtElem != null) {
            mTargetDocument = tgtElem.getOwnerDocument();
        } else {
            mTargetDocument = null;
        }
        mTransformer = transformer;
        mRootX = new RootXNode(fct);
        mRootX.mYNodes.add(new RootYNode(mRootX, fct));
    }

    public void setInplace(boolean val) {
        mInplace = val;
    }

    public boolean getInplace() {
        return mInplace;
    }

    public boolean checkSortNeeded()
            throws TransformerException, DOMException {
        NodeList childNodes = mSourceElement.getChildNodes();
        synchronized (childNodes) {
            ParticlePath lastPath = null;
            for (int i = 0; i < childNodes.getLength(); i++) {
                Node childNode = childNodes.item(i);
                if (childNode.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }
                try {
                    AggregatedParticle aggPart =
                            mFlatComplexType.getAggregatedParticles(
                            DOMUtil.getQName(childNode));

                    Collection<ParticleAppearance> partApps;
                    if (aggPart == null) {
                        aggPart =
                                mFlatComplexType.getAggregatedParticles(
                                SwareFlatComplexType.ELEMENT_WILDCARD_QNAME);
                        AggregatedWildcard aggWildcard =
                                (AggregatedWildcard) aggPart;
                        if (aggWildcard == null
                                || !aggWildcard.coversNamespace(
                                childNode.getNamespaceURI())) {
                            //there is no wildcard particle or its namespace does
                            //not cover the namespace of the element
                            if (mTransformer.getPolicies() != null
                                    && mTransformer.getPolicies().getPolicyByte(
                                    FOPolicy.ID)
                                    == FOPolicy.KEEP_ORIGINAL_ORDER) {
                                return false;
                            }
                            throw new ElementNotFoundInComplexTypeException(
                                    "Particle not found in the complex type. "
                                    + "element=" + DOMUtil.getQName(childNode)
                                    + ", complexType="
                                    + mFlatComplexType);
                        }
                        if (!aggWildcard.isLocalDeterministic()) {
                            //Not locally deterministic so there is no easy way
                            //to validate the order. Do sorting.
                            return true;
                        }
                        partApps =
                                aggWildcard.getCandidateParticles(
                                childNode.getNamespaceURI());
                    } else {
                        if (!aggPart.getParticleType().equals(
                                LeafParticleType.ELEMENTDECL)) {
                            //It should not be possible to reach here. Otherwise
                            //it is a programming error.
                            throw new IllegalStateException(
                                    "Illegal particle type: "
                                    + aggPart.getParticleType());
                        }
                        if (!aggPart.isLocalDeterministic()) {
                            //Not locally deterministic so there is no easy way
                            //to validate the order. Do sorting.
                            return true;
                        }
                        partApps = aggPart.getParticleAppearances();
                    }

                    if (partApps.size() > 1) {
                        return true;
                    }

                    ParticleAppearance partApp = partApps.iterator().next();
                    if (!partApp.orderVerifiable()) {
                        return true;
                    }

                    if (partApp.getPath().compareTo(lastPath) < 0) {
                        //Out of order
                        return true;
                    }

                    lastPath = partApp.getPath();

                    SwareType xmlType = null;
                    QName xsiType = null;
                    if ((xsiType = DOMUtil.getXsiType((Element) childNode))
                            != null) {
                        //xsi:type takes the precedence
                        xmlType =
                                mTransformer.getSwareTypeSystem().findType(xsiType);
                    } else {
                        SwareElement elemDecl = null;
                        if (LeafParticleType.ELEMENTDECL.equals(
                                partApp.getParticleType())) {
                            elemDecl = (SwareElement) partApp.getParticle();
                        } else {
                            elemDecl =
                                    mTransformer.getSwareTypeSystem().findElement(
                                    DOMUtil.getQName(childNode));
                        }
                        if (elemDecl != null) {
                            xmlType = elemDecl.getType();
                        }
                    }
                    if (xmlType == null || !xmlType.isComplexType()
                            || ((SwareComplexType) xmlType).isSimpleContent()) {
                        continue;
                    }
                    SwareFlatComplexType fct =
                            mTransformer.getSwareTypeSystem().getFlatComplexType(
                            (SwareComplexType) xmlType);
                    DOMSorter sorter = new DOMSorter(mTransformer, fct,
                            (Element) childNode, null);
                    if (sorter.checkSortNeeded()) {
                        return true;
                    }
                } catch (SwareSchemaException e) {
                    throw new InvalidSchemaException("Invalid Schema.", e);
                }
            }
        }
        return false;
    }

    /**
     * Does the sorting job.
     *
     * @throws TransformerException transformation exception
     * @throws DOMException DOM exception
     */
    public void sort() throws TransformerException, DOMException {

        NodeList childNodes = mSourceElement.getChildNodes();
        synchronized (childNodes) {
            List<Node> otherNodes = null;
            boolean noSort = false;
            int i = 0;
            while (childNodes.getLength() > i) {
                Node childNode = childNodes.item(i);
                if (childNode.getNodeType() != Node.ELEMENT_NODE) {
                    //preserve all non-element nodes
                    if (otherNodes == null) {
                        otherNodes = new ArrayList<Node>();
                    }
                    if (mInplace) {
                        otherNodes.add(
                                mSourceElement.removeChild(childNode));
                    } else {
                        otherNodes.add(childNode);
                        i++;
                    }
                    continue;
                }
                Element impChildNode;
                if (mInplace) {
                    impChildNode = (Element) mSourceElement.removeChild(childNode);
                } else {
                    impChildNode =
                            (Element) mTargetDocument.importNode(childNode, false);
                }
                try {
                    if (noSort) {
                        mUnsortedList.add(
                                new Tile((Element) childNode,
                                impChildNode, otherNodes));
                    } else {
                        Entry<Tile> tileRef =
                                addToSortedList((Element) childNode, impChildNode,
                                otherNodes);
                        mUnsortedList.add(tileRef.getElement());
                    }
                } catch (OrderIndeterministicException e) {
                    if (mTransformer.getPolicies() != null
                            && mTransformer.getPolicies().getPolicyByte(FOPolicy.ID)
                            == FOPolicy.THROW_EXCEPTION) {
                        throw e;
                    } else {
                        //Default handling: keeps original order
                        mTransformer.warning(e);
                    }
                    mUnsortedList.add(
                            new Tile((Element) childNode,
                            impChildNode, otherNodes));
                    noSort = true;
                } catch (MissingSchemaInfoException e) {
                    if (mTransformer.getPolicies() != null
                            && mTransformer.getPolicies().getPolicyByte(FOPolicy.ID)
                            == FOPolicy.KEEP_ORIGINAL_ORDER) {
                        mUnsortedList.add(
                                new Tile((Element) childNode,
                                impChildNode, otherNodes));
                        noSort = true;
                    } else {
                        throw e;
                    }
                } catch (SwareSchemaException e) {
                    throw new InvalidSchemaException("Invalid Schema.", e);
                }
                otherNodes = null;
                if (!mInplace) {
                    i++;
                }
            }
            try {
                List<Tile> tileList;
                if (noSort) {
                    tileList = mUnsortedList;
                } else {
                    tileList = mSortedList;
                }
                for (Tile tile : tileList) {
                    if (tile.getOtherNodes() != null) {
                        for (Node node : tile.getOtherNodes()) {
                            if (mInplace) {
                                mTargetElement.appendChild(node);
                            } else {
                                mTargetElement.appendChild(
                                        mTargetDocument.importNode(node, true));
                            }
                        }
                    }
                    Element src = tile.getSource();
                    Element tgtElem =
                            (Element) mTargetElement.appendChild(tile.getTarget());
                    if (!mInplace) {
                        DOMUtil.copyAttributes(src, tgtElem);
                    }
                    SwareType xmlType = null;
                    QName xsiType = null;
                    if ((xsiType = DOMUtil.getXsiType(tgtElem)) != null) {
                        //xsi:type takes the precedence
                        xmlType =
                                mTransformer.getSwareTypeSystem().findType(xsiType);
                    } else {
                        ParticleAppearance partApp;
                        SwareElement elemDecl = null;
                        if (tile instanceof SortedTile) {
                            partApp = ((SortedTile) tile).getParticleAppearance();
                            if (partApp.getParticleType().equals(
                                    LeafParticleType.ELEMENTDECL)) {
                                elemDecl = (SwareElement) partApp.getParticle();
                            } else if (partApp.getParticleType().equals(
                                    LeafParticleType.WILDCARD)) {
                                //Checks if the element is known to the type system
                                elemDecl =
                                        mTransformer.getSwareTypeSystem().findElement(
                                        DOMUtil.getQName(tgtElem));
                            }
                        } else {
                            partApp = null;
                            AggregatedParticle aggPart =
                                    mFlatComplexType.getAggregatedParticles(
                                    DOMUtil.getQName(tgtElem));
                            if (aggPart != null) {
                                partApp =
                                        aggPart.getParticleAppearances().size() > 0
                                        ? aggPart.getParticleAppearances().iterator().next() : null;
                            }
                            if (partApp != null) {
                                elemDecl = (SwareElement) partApp.getParticle();
                            } else {
                                elemDecl =
                                        mTransformer.getSwareTypeSystem().findElement(
                                        DOMUtil.getQName(tgtElem));
                            }
                        }
                        if (elemDecl != null) {
                            xmlType = elemDecl.getType();
                        }
                    }
                    if (xmlType == null || !xmlType.isComplexType()
                            || ((SwareComplexType) xmlType).isSimpleContent()) {
                        if (!mInplace) {
                            DOMUtil.copyAllChildNodes(src, tgtElem);
                        }
                        continue;
                    }
                    SwareFlatComplexType fct =
                            mTransformer.getSwareTypeSystem().getFlatComplexType(
                            (SwareComplexType) xmlType);
                    DOMSorter sorter = new DOMSorter(mTransformer, fct,
                            tile.getSource(), tgtElem);
                    sorter.setInplace(mInplace);
                    sorter.sort();
                }
            } catch (SwareSchemaException e) {
                throw new InvalidSchemaException("Invalid Schema.", e);
            }
            if (otherNodes != null) {
                for (Node node : otherNodes) {
                    if (mInplace) {
                        mTargetElement.appendChild(node);
                    } else {
                        mTargetElement.appendChild(
                                mTargetDocument.importNode(node, true));
                    }
                }
            }
        }
    }

    /**
     * Analyzes order and adds an element to the sorted list.
     *
     * @param source the source element
     * @param target the target element
     * @param others other DOM nodes than element nodes before this element
     * @return an instance of Entry<Tile> if adding to sorted list is successful
     * @throws OrderIndeterministicException thrown if the order is not
     * deterministic
     * @throws TooManyElementsException thrown if maxOccurs is reached
     * @throws SwareSchemaException thrown if the schema is detected to be
     * invalid
     */
    private Entry<Tile> addToSortedList(Element source, Element target,
            List<Node> others)
            throws OrderIndeterministicException, MissingSchemaInfoException,
            TooManyElementsException, SwareSchemaException {
        AggregatedParticle aggPart =
                mFlatComplexType.getAggregatedParticles(
                DOMUtil.getQName(target));

        Collection<ParticleAppearance> partApps;
        if (aggPart == null) {
            aggPart =
                    mFlatComplexType.getAggregatedParticles(
                    SwareFlatComplexType.ELEMENT_WILDCARD_QNAME);
            AggregatedWildcard aggWildcard = (AggregatedWildcard) aggPart;
            if (aggWildcard == null
                    || !aggWildcard.coversNamespace(
                    target.getNamespaceURI())) {
                //there is no wildcard particle or its namespace does not
                //cover the namespace of the element
                throw new ElementNotFoundInComplexTypeException(
                        "Particle not found in the complex type. "
                        + "element=" + DOMUtil.getQName(target)
                        + ", complexType="
                        + mFlatComplexType);
            }
            if (!aggWildcard.isLocalDeterministic()) {
                throw new OrderIndeterministicException(
                        "Order of the elements accepted by the wildcard"
                        + " is not deterministic in the "
                        + getLocationInfo());
            }
            partApps =
                    aggWildcard.getCandidateParticles(
                    target.getNamespaceURI());
        } else {
            if (!aggPart.getParticleType().equals(
                    LeafParticleType.ELEMENTDECL)) {
                //It should not be possible to reach here. Otherwise it is
                //a programming error.
                throw new IllegalStateException(
                        "Illegal particle type: " + aggPart.getParticleType());
            }
            if (!aggPart.isLocalDeterministic()) {
                AggregatedElemDecl aggElem =
                        (AggregatedElemDecl) aggPart;
                throw new OrderIndeterministicException(
                        "Order of the elements is not deterministic."
                        + " element declaration: " + aggElem.getName()
                        + " in the " + getLocationInfo());
            }
            partApps = aggPart.getParticleAppearances();
        }
        Entry<Tile> added = null;
        for (ParticleAppearance partApp : partApps) {
            if ((added = mRootX.addLeafParticle(partApp, source,
                    target, others)) != null) {
                break;
            }
        }
        if (added == null) {
            String msg = "Too many elements: ";
            if (target != null) {
                msg += ("{" + target.getNamespaceURI() + "}"
                        + target.getLocalName());
            }
            throw new TooManyElementsException(msg);
        }
        return added;
    }

    private String getLocationInfo() {
        if (mFlatComplexType.getName() != null) {
            return "complex type definition: "
                    + mFlatComplexType.getName().toString();
        }
        if (mFlatComplexType.getContainerElement() != null
                && mFlatComplexType.getContainerElement().getName() != null) {
            return "local complex type definition in element declaration: "
                    + mFlatComplexType.getContainerElement().getName().toString();
        }
        return "unknown location";
    }

    /**
     * Root level XNode.
     *
     * @see XNode
     */
    class RootXNode extends XNode {

        /**
         * Constructs from a group.
         *
         * @param g the group
         * @throws SwareSchemaException invalid schema exception
         */
        RootXNode(SwareGroup g) throws SwareSchemaException {
            super(null, g, -1);
        }

        /**
         * Determines where to add the new element into the sorted list and then
         * adds it, and also update the dynamic particle tree based on the new
         * element added.
         *
         * @param partApp the particle appearance instance
         * @param s the source element
         * @param e the target element
         * @param others other DOM nodes than DOM elements before this element
         *
         * @return non-null instance of Entry<Tile> if the element is
         * successfully added to the sorted list, otherwise <code>null</code>
         * @throws SwareSchemaException invalid schema exception
         */
        public Entry<Tile> addLeafParticle(ParticleAppearance partApp,
                Element s, Element e, List<Node> others)
                throws SwareSchemaException {
            ParticlePath path = partApp.getPath();
            int[] rip = crackOutRIPath(path);
            if (rip == null) {
                return null;
            }
            YNode yNode = this.mYNodes.get(0);
            Entry<Tile> tileRef = null;
            for (int i = 0; i < rip.length; i++) {
                GNode gNode;
                int pi = path.getParticleIndex(i);
                if (yNode.mXNodes[pi] == null) {
                    //missing XNode, need reverse add
                    gNode = yNode;
                } else if (yNode.mXNodes[pi].mYNodes.size() <= rip[i]) {
                    //missing YNode, need reverse add
                    gNode = yNode.mXNodes[pi];
                } else {
                    yNode = yNode.mXNodes[pi].mYNodes.get(rip[i]);
                    continue;
                }
                int k = i;
                do {
                    if (gNode instanceof YNode) {
                        tileRef =
                                gNode.localAdd(partApp, path.getParticleIndex(k),
                                s, e, others);
                    } else if (gNode instanceof XNode) {
                        tileRef =
                                gNode.localAdd(partApp, rip[k], s, e, others);
                    } else {
                        throw new IllegalStateException(
                                "Node type not recognized.");
                    }
                    if (tileRef != null) {
                        break;
                    }
                    gNode = gNode.mOwner;
                    if (gNode instanceof XNode) {
                        k--;
                    }
                } while (tileRef == null && gNode != null && k >= 0);
                if (tileRef != null) {
                    //forward update
                    for (; k < rip.length; k++) {
                        if (gNode instanceof YNode) {
                            gNode.updateAdd(path.getParticleIndex(k), tileRef);
                            gNode = ((YNode) gNode).mXNodes[path.getParticleIndex(k)];
                            gNode.updateAdd(rip[k], tileRef);
                            gNode = ((XNode) gNode).mYNodes.get(rip[k]);
                        } else if (gNode instanceof XNode) {
                            gNode.updateAdd(rip[k], tileRef);
                            gNode = ((XNode) gNode).mYNodes.get(rip[k]);
                        } else {
                            throw new IllegalStateException(
                                    "Node type not recognized.");
                        }
                    }
                    return tileRef;
                } else {
                    //Should not be possible since repeating indices path has
                    //been already cracked out in crackOutRIPath(), which
                    //means maximum occurrence has been reached yet.
                    //If reaches here, that means there must be some
                    //pragramming logic problems
                    throw new IllegalStateException(
                            "Local add failed at all levels");
                }
            }
            //Must be having programming logic problem
            throw new IllegalStateException(
                    "Unable to add element based on the particle index path"
                    + " and repeating index path.");
        }

        /**
         * Cracks out the new repeating indices path assuming that a new element
         * is going to be added.
         *
         * @param path the path of the particle appearance
         * @return the repeating indices path
         * @throws SwareSchemaException invalid schema exception
         */
        private int[] crackOutRIPath(ParticlePath path)
                throws SwareSchemaException {
            int[] riPath = new int[path.getNumOfLevels()];
            XNode xNode = mRootX;
            SwareGroup group = mFlatComplexType;
            SwareParticle[] partsOnPath = new SwareParticle[riPath.length];
            int lastIndex = 0;
            for (int i = 0; i < riPath.length; i++) {
                int pi = path.getParticleIndex(i);
                SwareParticle[] parts;
                if (group.isReference()) {
                    parts = group.getReference().getParticles();
                } else {
                    parts = group.getParticles();
                }
                partsOnPath[i] = parts[pi];
                BigInteger bound = parts[pi].getMaxOccurs();
                if (bound != null && bound.signum() == 0) {
                    //ZERO
                    return null;
                }
                if (xNode == null) {
                    riPath[i] = 0;
                } else {
                    if (i < riPath.length - 1) {
                        if (xNode.mPCounters[pi] == 0) {
                            riPath[i] = 0;
                        } else {
                            if (bound == null) {
                                riPath[i] = 0;
                            } else {
                                riPath[i] =
                                        (xNode.mPCounters[pi] - 1)
                                        / bound.intValue();
                            }
                        }
                    } else {
                        //the last level
                        if (bound == null) {
                            riPath[i] = 0;
                            lastIndex = xNode.mPCounters[pi];
                        } else {
                            if (xNode.mPCounters[pi] != 0
                                    && (bound.intValue() == 1
                                    || (xNode.mPCounters[pi] + 1)
                                    % bound.intValue() == 1)) {
                                riPath[i] =
                                        (xNode.mPCounters[pi] - 1)
                                        / bound.intValue();
                                int k = i;
                                BigInteger rBound;
                                //reverse adjust
                                while (k > 0) {
                                    rBound = partsOnPath[k - 1].getMaxOccurs();
                                    if (rBound == null
                                            || riPath[k] + 1
                                            < rBound.intValue()) {
                                        riPath[k]++;
                                        break;
                                    } else {
                                        riPath[k] = 0;
                                    }
                                    k--;
                                }
                                if (k == 0) {
                                    //cannot increase any more
                                    return null;
                                }
                            } else {
                                if (xNode.mPCounters[pi] == 0) {
                                    riPath[i] = 0;
                                } else {
                                    riPath[i] =
                                            xNode.mPCounters[pi]
                                            / bound.intValue();
                                }
                                lastIndex =
                                        xNode.mPCounters[pi] % bound.intValue();
                            }
                        }
                    }
                }
                if (xNode != null && riPath[i] < xNode.mYNodes.size()) {
                    xNode = xNode.mYNodes.get(riPath[i]).mXNodes[pi];
                }
                if (i < riPath.length - 1) {
                    group = (SwareGroup) parts[pi];
                }
            }
            for (int i = 1; i < riPath.length; i++) {
                riPath[i - 1] = riPath[i];
            }
            riPath[riPath.length - 1] = lastIndex;
            return riPath;
        }
    }

    /**
     * Generic node type for the dynamic particle tree.
     */
    abstract class GNode {

        final GNode mOwner;

        /**
         * Constructs from an owner.
         *
         * @param owner
         */
        GNode(GNode owner) {
            mOwner = owner;
        }

        /**
         * Gets the owner.
         *
         * @return the owner
         */
        public GNode getOwner() {
            return mOwner;
        }

        /**
         * Locally adds an element, which only searches for the posibility in
         * direct child or repeating nodes.
         *
         * @param partApp the particle appearance instance
         * @param i either the particle index or the repeating index
         * @param s the source element
         * @param e the target element
         * @param others other DOM nodes than DOM element nodes before this
         * element
         * @return A reference to a tile in the sorted list or <code>null</code>
         * if unable to insert
         */
        abstract Entry<Tile> localAdd(ParticleAppearance partApp, int i,
                Element s, Element e, List<Node> others);

        /**
         *
         * @param i
         * @param tileRef
         * @throws SwareSchemaException
         */
        abstract void updateAdd(int i, Entry<Tile> tileRef)
                throws SwareSchemaException;

        abstract Entry<Tile> getFirstTile();

        abstract Entry<Tile> getLastTile();
    }

    class RootYNode extends YNode {

        RootYNode(XNode owner, SwareGroup g) throws SwareSchemaException {
            super(owner, g, true);
        }
    }

    class YNode extends GNode {

        static final int NO_OP = 0;
        static final int ADD_BEFORE = 1;
        static final int ADD_AFTER = 2;
        final SwareParticle mPart;
        final SwareParticle[] mChildParts;
        final XNode[] mXNodes;
        Entry<Tile> mTileRef;
        final boolean mIsRoot;

        YNode(XNode owner, SwareParticle p) throws SwareSchemaException {
            this(owner, p, false);
        }

        YNode(XNode owner, SwareParticle p, boolean isRoot)
                throws SwareSchemaException {
            super(owner);
            mPart = p;
            mIsRoot = isRoot;
            if (p.getParticleType() == ParticleType.GROUP) {
                SwareGroup g = (SwareGroup) p;
                if (g.isReference()) {
                    g = g.getReference();
                }
                mChildParts = g.getParticles();
                mXNodes = new XNode[mChildParts.length];
            } else {
                mChildParts = new SwareParticle[0];
                mXNodes = new XNode[0];
            }
        }

        SwareParticle getParticle() {
            return mPart;
        }

        Entry<Tile> localAdd(ParticleAppearance partApp, int i,
                Element s, Element e, List<Node> others) {
            if (mXNodes.length <= 1 && !mIsRoot) {
                return null;
            }
            if (i < 0 || i >= mXNodes.length) {
                throw new ArrayIndexOutOfBoundsException(i);
            }
            int action = NO_OP;
            Entry<Tile> ref = null;
            if (i != mXNodes.length - 1) {
                //search forward only
                for (int k = i + 1; k < mXNodes.length; k++) {
                    if (mXNodes[k] != null) {
                        action = ADD_BEFORE;
                        ref = mXNodes[k].getFirstTile();
                        break;
                    }
                }
            }
            if (action == NO_OP && i != 0) {
                //search backward only
                for (int k = i - 1; k >= 0; k--) {
                    if (mXNodes[k] != null) {
                        action = ADD_AFTER;
                        ref = mXNodes[k].getLastTile();
                        break;
                    }
                }
            }
            if (action == NO_OP && !mIsRoot) {
                return null;
            }
            Tile newTile = new SortedTile(s, e, others, partApp);
            if (action == ADD_AFTER) {
                return mSortedList.addAfter(newTile, ref);
            } else if (action == ADD_BEFORE) {
                return mSortedList.addBefore(newTile, ref);
            } else if (mIsRoot) {
                //sorted list must be empty
                return mSortedList.addFirstEntry(newTile);
            }
            //should not be possible
            throw new IllegalStateException("Illegal action code = " + action);
        }

        @Override
        void updateAdd(int i, Entry<Tile> tileRef) throws SwareSchemaException {
            if (i < 0 || i >= mXNodes.length) {
                throw new ArrayIndexOutOfBoundsException(i);
            }
            if (mXNodes[i] == null) {
                mXNodes[i] = new XNode(this, mChildParts[i], i);
            }
        }

        @Override
        Entry<Tile> getFirstTile() {
            if (mXNodes.length == 0) {
                return mTileRef;
            } else {
                for (int i = 0; i < mXNodes.length; i++) {
                    if (mXNodes[i] != null) {
                        return mXNodes[i].getFirstTile();
                    }
                }
            }
            if (!mIsRoot) {
                throw new IllegalStateException(
                        "Any non-root YNode must at least have one tile.");
            }
            return null;
        }

        @Override
        Entry<Tile> getLastTile() {
            if (mXNodes.length == 0) {
                return mTileRef;
            } else {
                for (int i = mXNodes.length - 1; i >= 0; i--) {
                    if (mXNodes[i] != null) {
                        return mXNodes[i].getLastTile();
                    }
                }
            }
            if (!mIsRoot) {
                throw new IllegalStateException(
                        "Any non-root YNode must at least have one tile.");
            }
            return null;
        }
    }

    class XNode extends GNode {

        final int[] mPCounters;
        final List<YNode> mYNodes = new ArrayList<YNode>();
        final SwareParticle mPart;
        final int mPartIndex;

        XNode(YNode owner, SwareParticle p, int partIndex)
                throws SwareSchemaException {
            super(owner);
            mPart = p;
            mPartIndex = partIndex;
            if (p.getParticleType() == ParticleType.GROUP) {
                SwareGroup g = (SwareGroup) p;
                if (g.isReference()) {
                    g = g.getReference();
                }
                mPCounters = new int[g.getParticles().length];
                Arrays.fill(mPCounters, 0);
            } else {
                mPCounters = new int[0];
            }
        }

        Entry<Tile> localAdd(ParticleAppearance partApp, int i,
                Element s, Element e, List<Node> others) {
            if (i == 0) {
                return null;
            }
            Tile newTile = new SortedTile(s, e, others, partApp);
            return mSortedList.addAfter(newTile,
                    mYNodes.get(i - 1).getLastTile());
        }

        @Override
        void updateAdd(int i, Entry<Tile> tileRef) throws SwareSchemaException {
            if (i > mYNodes.size()) {
                //This should not happen
                throw new IllegalStateException(
                        "i value " + i + " is greater than size "
                        + mYNodes.size());
            }
            if (i == mYNodes.size()) {
                YNode yNode = new YNode(this, mPart);
                if (yNode.mXNodes.length == 0) {
                    yNode.mTileRef = tileRef;
                }
                mYNodes.add(i, yNode);
                if (mOwner.mOwner != null) {
                    ((XNode) mOwner.mOwner).mPCounters[mPartIndex]++;
                }
            }
        }

        @Override
        Entry<Tile> getFirstTile() {
            if (mYNodes.size() == 0) {
                throw new IllegalStateException(
                        "An XNode must have at least one YNode.");
            }
            return mYNodes.get(0).getFirstTile();
        }

        @Override
        Entry<Tile> getLastTile() {
            if (mYNodes.size() == 0) {
                throw new IllegalStateException(
                        "An XNode must have at least one YNode.");
            }
            return mYNodes.get(mYNodes.size() - 1).getLastTile();
        }
    }

    private static class Tile {

        protected final Element mSource;
        protected final Element mTarget;
        protected final List<Node> mOthers;

        /**
         * Constructs from source element, target element and a list of other
         * nodes that are before the source node.
         *
         * @param source source element
         * @param target target element
         * @param others other nodes before the source element
         */
        private Tile(Element source, Element target, List<Node> others) {
            mSource = source;
            mTarget = target;
            mOthers = others;
        }

        public Element getSource() {
            return mSource;
        }

        public Element getTarget() {
            return mTarget;
        }

        public List<Node> getOtherNodes() {
            return mOthers;
        }
    }

    private static class SortedTile extends Tile {

        protected final ParticleAppearance mPartApp;

        /**
         * Constructs from source element, target element, a list of other nodes
         * that are before the source node and a particle appearance.
         *
         * @param source source element
         * @param target target element
         * @param others other nodes before the source element
         * @param partApp the particle appearance applied during sorting
         */
        private SortedTile(Element source, Element target,
                List<Node> others, ParticleAppearance partApp) {
            super(source, target, others);
            mPartApp = partApp;
        }

        public ParticleAppearance getParticleAppearance() {
            return mPartApp;
        }
    }
}
