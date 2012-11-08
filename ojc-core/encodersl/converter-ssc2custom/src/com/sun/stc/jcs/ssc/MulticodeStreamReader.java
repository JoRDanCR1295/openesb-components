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
 * @(#)ByteArraySeekInputStream.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.IOException;

import com.sun.stc.jcs.ssc.SeekInputStream;
import com.sun.stc.jcs.ssc.Sjis;
import com.sun.stc.jcs.ssc.Uhc;

/**
 * Seekable input stream class, based on a byte array as the underlying
 * storage unit.
 */
class ByteArraySeekInputStream implements SeekInputStream
{ 
    private byte[] buf = null;
    private int pos = -1;

    /**
     * Creates from byte array.
     *
     * @param buf  an immutable array of bytes
     */
    public ByteArraySeekInputStream (byte [] buf)
    {
	this.buf = buf;
	this.pos = 0;
    }

    /**
     * Returns information on position.
     * The result can be used as the argument to seek(long).
     *
     * @return the position, or -1 after close()
     */
    public long tell ()
    {
	return (buf == null ? -1 : pos);
    }

    /**
     * Returns stream cursor to given position.
     * The position is normally obtained by an earlier call to tell().
     *
     * @param pos  the new position
     * @throwns IOException after close() or with invalid position
     */
    public void seek (long pos)
	throws IOException
    {
	if (buf == null)
	    throw new IOException("seeking on closed SeekInputStream");
	if (pos < 0 || buf.length < pos)
	    throw new IOException("seek (" + pos
		+ ") out of range (0," + buf.length + ")");
	this.pos = (int) pos;
    }

    /**
     * Stream cursor skips next N bytes (negative is backward).
     *
     * @param offset  number of bytes to skip
     * @throws IOException after close() or invalid offset
     */
    public void skip (long offset)
	throws IOException
    {
	if (buf == null)
	    throw new IOException("skipping on closed SeekInputStream");
	long rest = buf.length - pos;
	if (offset < -pos || rest < offset)
	    throw new IOException("skip (" + offset
		+ ") out of range (" + (-pos) + "," + rest + ")");
	this.pos += offset;
    }

    /**
     * Fetches next input byte at cursor, or EOF.
     *
     * @return byte value 0-255, or SeekInputStream.EOF
     */
    public int read ()
    {
	if (buf == null || buf.length <= pos)
	    return SeekInputStream.EOF;
	return (buf[pos ++] & 0xFF);
    }

    /**
     * Tries to fetch next N bytes, returns real count (which may be less).
     *
     * @param buf  the fetch buffer
     * @param offset  offset into fetch buffer, for start of result
     * @param count  number of bytes to fetch
     * @return the number of bytes really read
     */
    public int read (byte[] buf, int offset, int count)
    {
	if (buf == null)
	    return -1;
	int rest = buf.length - pos;
	if (rest < count)
	    count = rest;
	for (int i = 0; i < rest; i ++)
	    buf[i + offset] = this.buf[i + pos];
	pos += count;
	return count;
    }

    /**
     * Closes the stream, releases any resources held.
     *
     * @throws IOException when called twice
     */
    public void close ()
	throws IOException
    {
	if (buf == null)
	    throw new IOException
		("re-closing ByteArraySeekInputStream");
	buf = null;
	pos = -1;
    }
}

/**
 * Seekable input stream class, based on a file reader as the underlying
 * storage unit.
 */
class FileSeekInputStream implements SeekInputStream
{
    private java.io.RandomAccessFile in = null;
    private String name;

    /**
     * Creates from byte array.
     *
     * @param buf  an immutable array of bytes
     */
    public FileSeekInputStream (String name)
	throws IOException
    {
	this.in = new java.io.RandomAccessFile(name, "r");
	this.name = name;
    }

    /**
     * Returns information on position.
     * The result can be used as the argument to seek(long).
     *
     * @return the position, or -1 after close()
     */
    public long tell ()
	throws IOException
    {
	return (in == null ? -1 : in.getFilePointer());
    }

    /**
     * Returns stream cursor to given position.
     * The position is normally obtained by an earlier call to tell().
     *
     * @param pos  the new position
     * @throwns IOException after close() or with invalid position
     */
    public void seek (long pos)
	throws IOException
    {
	if (in == null)
	    throw new IOException("seeking on closed SeekInputStream");
	in.seek(pos);
    }

    /**
     * Stream cursor skips next N bytes (negative is backward).
     *
     * @param offset  number of bytes to skip
     * @throws IOException after close() or invalid offset
     */
    public void skip (long offset)
	throws IOException
    {
	if (in == null)
	    throw new IOException("skipping on closed SeekInputStream");
	if ((int) offset != offset)
	    throw new IOException("SeekInputStream.skip: overflow");
	in.skipBytes((int) offset);
    }

    /**
     * Fetches next input byte at cursor, or EOF.
     *
     * @return byte value 0-255, or SeekInputStream.EOF
     */
    public int read ()
	throws IOException
    {
	return (in == null ? -1 : in.read());
    }

    /**
     * Tries to fetch next N bytes, returns real count (which may be less).
     *
     * @param buf  the fetch buffer
     * @param offset  offset into fetch buffer, for start of result
     * @param count  number of bytes to fetch
     * @return the number of bytes really read
     */
    public int read (byte[] buf, int offset, int count)
	throws IOException
    {
	return (in == null ? -1 : in.read(buf, offset, count));
    }

    /**
     * Closes the stream, releases any resources held.
     *
     * @throws IOException when called twice
     */
    public void close ()
	throws IOException
    {
	if (in == null)
	    throw new IOException
		("re-closing ByteArraySeekInputStream");
	this.in.close();
	this.in = null;
	this.name = null;
    }
}

/**
 * Class similar to InputStreamReader, to read character data from a byte
 * stream, but with dynamically switchable encoding.  Requires a more flexible
 * underlying layer: SeekInputStream instead of InputStream.
 *
 * Future direction: Currently, the decoding methods are rather hard-coded
 * in this class itself.  It would be better to make this pluggable, either
 * be deriving subclasses or providing some interface, to delegate the
 * implementation of read() to an encoding-dependent function, without
 * sacrificing too much speed.
 */
public class MulticodeStreamReader
{ 
    private final static boolean debug = false;

    // Return-value for read() to signal end of input.
    public static final int EOF = -1;

    /* Minimum required buffer size (bytes), enough to any encoded
     * character value in any of the supported encodings.
     */
    private static final int bufSizeMin = 6;

    private SeekInputStream sis = null;
    private byte[] buf = null; // internal fetch buffer
    private long pos = 0; // sis's position at buf[0]
    private int top = 0; // buf[0..top-1] is filled
    private int at = 0; // buf[0..at-1] already processed
    private boolean eof = false; // did read EOF?
    private byte code = RAWBYTES; // current encoding

    // Modal state.
    boolean euc_in = false;

    // Enumeration for supported encodings.
    public final static byte RAWBYTES   = 0;
    public final static byte USASCII    = 1; // 7-bits only
    public final static byte ISO_8859_1 = 2; // a.k.a. Latin-1
    public final static byte UTF_8      = 3; // Unicode & ISO-10646
    public final static byte SHIFT_JIS  = 4; // Japanese
    public final static byte EUC_JIS    = 5; // Japanese (cf. ISO-2022)
    public final static byte MS949      = 6; // Korean
    public final static byte BIG_5      = 7; // Chinese

    /**
     * Representation for a seek-position.  This must include any
     * modal state of the decoding process at that point.
     */
    public static class MulticodePos
    {
	long pos;
	int offset;
	byte code;
	boolean euc_in;

	/**
	 * Create new position info from internal state.
	 * Outside classes should not call this directly, but use tell().
	 * Note that "euc_in" is a hack, which should be generalized to
	 * user-supplied encoder state some day...
	 *
	 * @param pos  cursor position in underlying input byte stream
	 * @param offset  current offset in buffered bytes
	 * @param code  current encoding
	 * @param euc_in  auxuiliary code shift state for EUC
	 */
	public MulticodePos (long pos, int offset, byte code, boolean euc_in)
	{
	   this.pos = pos;
	   this.offset = offset;
	   this.code = code;
	   this.euc_in = euc_in;
	}

	/**
	 * Get distance from given prior seek value, in bytes.
	 * If the prior value has a greater offset, return the distance
	 * as a negative value.
	 *
	 * @param from  prior position to compare to
	 * @return the difference
	 */
	public long offset (MulticodePos from)
	{
	    return (pos + offset) - (from.pos + from.offset);
	}
    }

    /**
     * Creates from an underlying seekable input byte stream.
     *
     * @throws IOException for input stream errors
     */
    public MulticodeStreamReader (SeekInputStream sis, int bufSize)
	throws IOException
    {
	this.sis = sis;
	this.pos = sis.tell();
	this.top = this.at = 0;
	this.buf = new byte[java.lang.Math.max(bufSizeMin, bufSize)];
    }

    /**
     * Gets current position, including encoding state.
     *
     * @throws IOException for input stream errors
     */
    public MulticodePos tell ()
	throws IOException
    {
	return new MulticodePos(pos, at, code, euc_in);
    }

    /**
     * Sets position, as previously returned by tell().  This also flushes
     * the input buffer.  Note that the tell/seek offset values are independent
     * of the encoding, but they do preserve the current encoding as part of
     * the state.  This means a seek() call restores the current encoding.
     *
     * @param pos  required new position, including encoding state
     * @throws IOException for input stream errors or an invalid position
     */
    public void seek (MulticodePos pos)
	throws IOException
    {
	if (pos.pos != this.pos || pos.offset > this.top)
	{
	    // Different fragment in buffer; seek to it.
	    sis.seek(pos.pos);
	    sis.skip(pos.offset);
	    this.pos = sis.tell();
	    this.at = this.top = 0;
	    this.euc_in = pos.euc_in;
	}
	else
	{
	    // Reposition in current fragment.
	    this.at = pos.offset;
	    this.euc_in = pos.euc_in;
	}
	this.code = pos.code;
    }

    /**
     * Get next byte from buffer; refill if empty.  Return 0-255, or -1=EOF.
     *
     * @throws IOException for input stream block-read errors
     */
    private int nextByte ()
	throws IOException
    {
	if (at >= top)
	{
	    if (! eof)
	    {
		// Fetch more input.
		at = 0;
		pos = sis.tell();
		top = sis.read(buf, 0, buf.length);
		if (top <= 0)
		    eof = true;
	    }
	    if (eof)
	    {
		// End of data.
		return -1;
	    }
	}
	readBytes ++;
	return (buf[at ++] & 0xFF);
    }

    /**
     * Gets current encoding.
     * Not meaningful after a call to close().
     */
    public byte getEncoding ()
    {
	return code;
    }

    /**
     * Sets current encoding; defaults any modal state.
     * Note: I'm not happy about all encodings being hard-coded in this class.
     * In future, this may be replaced by a user-extensible mechanism.
     *
     * @param code  required encoding; must be from the list RAWBYTES etc.
     * @throws UnsupportedEncodingException for invalid code
     */
    public void setEncoding (byte code)
	throws java.io.UnsupportedEncodingException
    {
	switch (code)
	{
	case RAWBYTES:
	case USASCII:
	case UTF_8:
	case ISO_8859_1:
	case SHIFT_JIS:
	case MS949:
	case BIG_5:
	    // Okay.
	    this.code = code;
	    break;

	case EUC_JIS:
	    // Assume mode starts non-shifted.
	    this.code = code;
	    this.euc_in = false;
	    break;

	default:
	    throw new java.io.UnsupportedEncodingException
		("unknown encoding " + code);
	}
    }

    private MulticodePos ungetPos = new MulticodePos(0, 0, RAWBYTES, false);
    public int readBytes = 0; // last read()'s byte count

    /**
     * Get next character, translated to Unicode, or SeekInputStream.EOF.
     * As a side-effect, "readBytes" is set to the byte-count of this char.
     *
     * @throws IOException for input stream errors or code conversion errors
     */
    public int read ()
	throws IOException
    {
	int b1, b2, b3, c;

	// Common start; get lead byte, stop on EOF.
	readBytes = 0;
	b1 = nextByte();
	if (b1 < 0)
	    return EOF;

	switch (code)
	{
	case RAWBYTES:
	case ISO_8859_1:
	    // Raw bytes map to Unicode 0x00 - 0xFF.
	    // ISO-8859-1 (a.k.a. Latin-1) is the lowest page in Unicode.
	    return b1;

	case USASCII:
	    /* Only 7-bits values allowed.
	     * Ascii values map directly to Unicode 0x00-0x7F.
	     */
	    if (b1 > 0x7F)
		throw new IOException("MulticodeStreamReader: "
		    + "non-USASCII value (" + b1 + ")");
	    return b1;

	case UTF_8:
	    /* Don't bother checking for non-minimal encoding.
	     * UTF-8 is a straight ASCII-transparent encoding for Unicode;
	     * the top bits of the lead byte determine the length:
	     * 0xxxxxxx - for values U+0000 throug U+007F
	     * 110xxxxx 10xxxxxx - for values U+0080 through U+07FF
	     * 1110xxxx 10xxxxxx 10xxxxxx - for U+0800 through U+FFFC
	     */
	    if (b1 < 0)
		return EOF;
	    if ((b1 & 0x80) == 0x00)
	    {
		// Bit-pattern 0xxxxxxx: single-byte code.
		return b1;
	    }
	    if ((b1 & 0xE0) == 0xC0)
	    {
		// Bit-pattern 110xxxxx: double-byte code.
		b2 = nextByte();
		if (b2 < 0 || (b2 & 0xC0) != 0x80)
		    throw new IOException("MulticodeStreamReader: "
			+ "non-UTF-8 sequence " + b1 + "," + b2);
		return ((b1 & 0x1F) << 6) | (b2 & 0x3F);
	    }
	    if ((b1 & 0xF0) == 0xE0)
	    {
		// Bit-pattern 1110xxxx: triple-byte code.
		b2 = nextByte();
		if (b2 < 0 || (b2 & 0xC0) != 0x80)
		    throw new IOException("MulticodeStreamReader: "
			+ "non-UTF-8 sequence " + b1 + "," + b2);
		b3 = nextByte();
		if (b2 < 0 || (b2 & 0xC0) != 0x80)
		    throw new IOException("MulticodeStreamReader: "
			+ "non-UTF-8 sequence " + b1 + "," + b2 + "," + b3);
		return ((b1 & 0x0F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F);
	    }
	    // Pattern 10xxxxxx or 1111xxxx: illegal or overlong.
	    throw new IOException("MulticodeStreamReader: "
		+ "non-UTF-8 start-byte " + b1);

	case SHIFT_JIS:
	    /* Japanese Shift-JIS, mapping according to us.
	     * Note that we don't strictly want Cp932, i.e. do not map
	     * SJIS [\] to [Yen] symbol in Unicode, and do allow 0x80
	     * to map to the [Euro] symbol.  Strictly speaking, Shift-JIS
	     * is not roundtrip convertible to Unicode.
	     */
	    c = Sjis.sjis1[b1];
	    if (c == 0xFFFD)
		throw new IOException("MulticodeStreamReader: "
		    + "non-SJIS sequence " + b1);
	    if (c < 0)
	    {
		// Two-byte sequence.
		b2 = nextByte();

		c = (b2 < 0x40 ? 0xFFFD : Sjis.sjis2[b2 - 0x40 - c]);
		if (debug)
		{
		    System.out.print("<"
			+ Integer.toHexString(b1) + ":"
			+ Integer.toHexString(b2) + "="
			+ Integer.toHexString(c) + ">"
			);
		}
		if (c == 0xFFFD)
		    throw new IOException("MulticodeStreamReader: "
			+ "non-SJIS sequence " + b1 + "," + b2);
	    }
	    return c;

	case MS949:
	    /* Korean MS949 (UHC), mapping according to us.
	     * Strictly speaking, UHC  is not roundtrip convertible to Unicode.
	     */
	    c = Uhc.uhc1[b1];
	    if (c == 0xFFFD)
		throw new IOException("MulticodeStreamReader: "
		    + "non-UHC sequence " + b1);
	    if (c < 0)
	    {
		// Two-byte sequence.
		b2 = nextByte();
		c = (b2 < 0x40 ? 0xFFFD : Uhc.uhc2[b2 - 0x40 - c]);
		/*-
		System.out.print("<"
		    + Integer.toHexString(b1) + ":"
		    + Integer.toHexString(b2) + "="
		    + Integer.toHexString(c) + ">");
		-*/
		if (c == 0xFFFD)
		    throw new IOException("MulticodeStreamReader: "
			+ "non-UHC sequence " + b1 + "," + b2);
	    }
	    return c;

	case BIG_5:
	case EUC_JIS:
	    /* EUC (Extended Unix Code) is a portmanteau code for mixing
	     * up to 4 codesets in one stream; cf. ISO-2022. Lead bytes:
	     * 0xxxxxxx - code 0
	     * 1xxxxxxx (except 0x8E and 0x8F) 1... - code 1
	     * 10001110 1... - code 2
	     * 10001111 1... - code 3
	     * where "1..." stands for any number of bytes determined by the
	     * chosen encodings (together representing a single character),
	     * all with the highest bit set to "1".
	     * For EUC-JX, the codes are: ASCII, JIS-X-0208-1983,
	     * JIS-X-0201-1976, and JIS-X-0212-1990.
	     */
	    //NYI...
	default:
	    throw new RuntimeException("MulticodeStreamReader: "
		+ "encoding " + code + " not yet implemented");
	}
    }

    /**
     * Like read(), but the caller can follow this up with a single
     * call to unget() to undo the read.  Only works for the last character.
     *
     * @return Unicode character value, or SeekInputStream.EOF
     * @throws IOException for input stream errors or code conversion errors
     */
    public int readGet ()
	throws IOException
    {
	ungetPos.code = this.code;
	ungetPos.pos = pos;
	ungetPos.offset = at;
	ungetPos.euc_in = euc_in;
	return read();
    }

    /**
     * Undoes last readGet() call.  Only one character deep.
     *
     * @throws IOException for input stream errors, or without prior readGet()
     */
    public void unget ()
	throws IOException
    {
	if (ungetPos.offset < 0)
	    throw new IOException
		("MulticodeStreamReader.unget: no preceding readGet() call");
	seek(ungetPos);
    }

    /**
     * Closes this and underlying input stream.
     * This releases any resources held.
     *
     * @throws IOException when called twice or can't close
     */
    public void close ()
	throws IOException
    {
	if (sis != null)
	    sis.close();
	sis = null;
    }

    /**
     * Usage: main  encoding file
     * For testing only.
     */
    public static void main (String[] args)
    {
	try
	{
	    if (args.length != 2)
		throw new RuntimeException("Usage:  main <encoding> <file>");
	    FileSeekInputStream fis = new FileSeekInputStream(args[1]);
	    MulticodeStreamReader msr = new MulticodeStreamReader(fis, 1000);
	    msr.setEncoding((byte) Integer.parseInt(args[0]));
	    while (true)
	    {
		int ch = msr.read();
		if (ch == EOF) break;
		if (0x20 <= ch && ch <= 0x7E && ch != '\\')
		    System.out.print((char) ch);
		else
		{
		    String s = java.lang.Integer.toHexString(ch + 0x10000);
		    System.out.print("\\" + "u" +
			s.substring(s.length() - 4, s.length()).toUpperCase());
		}
		if (ch == '\n')
		    System.out.println();
	    }
	    System.out.println();
	}
	catch (Exception e)
	{
	    e.printStackTrace(System.err);
	}
    }
}
