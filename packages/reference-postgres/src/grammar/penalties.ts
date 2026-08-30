// Grammar penalties module
// Extracted from dict-grammar.ts (Lines 1896-2003)

import type { Segment, SegmentList } from '../types.js';
import type { Synergy, PenaltyFunction } from './types.js';
import { getText } from '../dict/utils.js';
import { SEMI_FINAL_PRT } from '../dict/errata.js';

// Line 961: defparameter *penalty-list*
const penaltyList: PenaltyFunction[] = [];

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

// Line 963-967: defmacro defpenalty
function defpenalty(_name: string, fn: PenaltyFunction): void {
  if (!penaltyList.includes(fn)) {
    penaltyList.unshift(fn); // Add to beginning like Lisp's pushnew
  }
}

// Line 969-981: defmacro def-generic-penalty
function defGenericPenalty(
  name: string,
  filterLeft: (segment: SegmentList) => boolean,
  filterRight: (segment: SegmentList) => boolean,
  options: {
    description: string;
    connector?: string;
    score: number;
    serial?: boolean;
  }
): void {
  const { description, connector = ' ', score, serial = true } = options;

  const penaltyFn: PenaltyFunction = async (segmentListLeft, segmentListRight) => {
    const start = segmentListLeft.end;
    const end = segmentListRight.start;

    // Line 975: (when (and ,(if serial `(= ,start ,end) t) ...))
    if (serial && start !== end) {
      return null;
    }

    if (filterLeft(segmentListLeft) && filterRight(segmentListRight)) {
      return { start, end, description, connector, score };
    }

    return null;
  };

  defpenalty(name, penaltyFn);
}

// Line 983-991: defun filter-short-kana
function filterShortKana(len: number, options: { except?: string[] } = {}): (segment: SegmentList) => boolean {
  const { except } = options;

  return (segmentList: SegmentList) => {
    const seg = segmentList.segments[0];
    if (!seg) return false;

    // Line 988-990: (<= (- (segment-list-end) (segment-list-start)) len)
    const lengthOk = (segmentList.end - segmentList.start) <= len;
    if (!lengthOk) return false;

    // Line 990: (not (car (getf (segment-info seg) :kpcl)))
    const info = (seg as any).info || {};
    if (info.kpcl?.[0]) return false;

    // Line 991: (not (and except (member (get-text seg) except :test 'equal)))
    if (except && except.includes(getText(seg))) return false;

    return true;
  };
}

// Helper: filter segments by seq set
function filterInSeqSet(...seqs: number[]): (segment: Segment) => boolean {
  return (segment: Segment) => {
    const info = segment.info;
    if (!info) return false;

    const seqSet = info.seqSet || [];
    return seqs.some(seq => seqSet.includes(seq));
  };
}

// ============================================================================
// PENALTY DEFINITIONS
// ============================================================================

// Line 993-998: def-generic-penalty penalty-short
defGenericPenalty(
  'penalty-short',
  filterShortKana(1, {}),
  filterShortKana(1, { except: ['と'] }),
  {
    description: 'short',
    serial: false,
    score: -9,
  }
);

// Line 1000-1006: def-generic-penalty penalty-semi-final
defGenericPenalty(
  'penalty-semi-final',
  (sl: SegmentList) => {
    // Line 1002: (some (lambda (s) (funcall (apply 'filter-in-seq-set *semi-final-prt*) s)) ...)
    return sl.segments.some(s => filterInSeqSet(...SEMI_FINAL_PRT)(s));
  },
  (_: SegmentList) => true,
  {
    description: 'semi-final not final',
    score: -15,
  }
);

// ============================================================================
// PUBLIC API
// ============================================================================

// Line 1008-1013: defun get-penalties
export async function getPenalties(segLeft: SegmentList, segRight: SegmentList): Promise<[SegmentList, Synergy, SegmentList] | [SegmentList, SegmentList]> {
  // Line 1009-1012: (loop for fn in *penalty-list* for penalty = (funcall fn seg-left seg-right) ...)
  for (const fn of penaltyList) {
    const penalty = await fn(segLeft, segRight);
    if (penalty) {
      // Line 1012: do (return (list seg-right penalty seg-left))
      return [segRight, penalty, segLeft];
    }
  }

  // Line 1013: finally (return (list seg-right seg-left))
  return [segRight, segLeft];
}
