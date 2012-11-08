package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.xmpp.extensions.XMPPConstants;

import org.jdom.Element;
import org.jdom.Namespace;

import org.jdom.output.XMLOutputter;

import org.jivesoftware.smack.packet.Presence;

import java.util.Map;
import java.util.Set;


/**
 * Handles the wrapping of a complex message type.
 * @author cgallemore
 * @since<pre>Sep 19, 2007<pre>
 */
public final class XMPPNormalizer {
    /**
     * Takes the given Map and creates a complex type that contains information
     * about occupants that are in a given room and what their presence is.
     * @param presence - Map that contains the Presence object for a given occupant of
     *                   a room.  The keys are the the occupants.
     * @return - String that is a complex message type conforming to the OccupantList type
     *           defined in the xmppbcext.xsd.
     */
    public static String createNormalizedOccupantPresence(
        Map<String, Presence> presence) {
        XMLOutputter xmlOutputter = new XMLOutputter();

        Namespace namespace = Namespace.getNamespace(XMPPConstants.XMPP_SCHEMA_NS);
        Element occupantListElement = new Element("OccupantList", namespace);

        Set<String> keys = presence.keySet();

        for (String key : keys) {
            Presence occuPresence = presence.get(key);
            Element occupantElement = new Element("Occupant", namespace);
            Element innerOccupantElement = new Element("occupant", namespace);
            Element presenceElement = new Element("presence", namespace);

            innerOccupantElement.setText(key);
            presenceElement.setText(occuPresence.toString());

            occupantElement.addContent(innerOccupantElement);
            occupantElement.addContent(presenceElement);
            occupantListElement.addContent(occupantElement);
        }

        return xmlOutputter.outputString(occupantListElement);
    }
}
