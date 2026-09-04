import { startTransition, useCallback, useEffect, useRef, useState } from 'react';

export interface TokenSelection {
  readonly start: number;
  readonly end: number;
}

export function useTokenSelection(): {
  readonly selection: TokenSelection | null;
  readonly selecting: boolean;
  readonly pointerDown: (index: number) => void;
  readonly pointerEnter: (index: number) => void;
  readonly pointerUp: () => void;
  readonly toggle: (index: number) => void;
  readonly select: (index: number) => void;
  readonly clear: () => void;
} {
  const [selection, setSelection] = useState<TokenSelection | null>(null);
  const [anchor, setAnchor] = useState<number | null>(null);
  const [dragging, setDragging] = useState(false);
  const lastPointerIndex = useRef<number | null>(null);

  const pointerDown = useCallback((index: number): void => {
    lastPointerIndex.current = index;
    setAnchor(index);
    setDragging(false);
  }, []);

  const pointerEnter = useCallback((index: number): void => {
    lastPointerIndex.current = index;
    if (anchor === null || index === anchor) return;
    setDragging(true);
    setSelection({ start: Math.min(anchor, index), end: Math.max(anchor, index) });
  }, [anchor]);

  const pointerUp = useCallback((): void => {
    const index = lastPointerIndex.current;
    if (index !== null && !dragging && anchor === index) {
      startTransition(() => {
        setSelection(current => current?.start === index && current.end === index
          ? null
          : { start: index, end: index });
      });
    }
    lastPointerIndex.current = null;
    setAnchor(null);
    setDragging(false);
  }, [anchor, dragging]);

  const clear = useCallback((): void => {
    setSelection(null);
    lastPointerIndex.current = null;
    setAnchor(null);
    setDragging(false);
  }, []);

  const select = useCallback((index: number): void => {
    setSelection({ start: index, end: index });
    lastPointerIndex.current = null;
    setAnchor(null);
    setDragging(false);
  }, []);

  const toggle = useCallback((index: number): void => {
    setSelection(current => current?.start === index && current.end === index
      ? null
      : { start: index, end: index });
  }, []);

  useEffect(() => {
    const stop = (): void => {
      lastPointerIndex.current = null;
      setAnchor(null);
      setDragging(false);
    };
    const cancel = (): void => {
      setSelection(null);
      stop();
    };
    window.addEventListener('pointerup', stop);
    window.addEventListener('pointercancel', cancel);
    return () => {
      window.removeEventListener('pointerup', stop);
      window.removeEventListener('pointercancel', cancel);
    };
  }, []);

  return {
    selection,
    selecting: anchor !== null,
    pointerDown,
    pointerEnter,
    pointerUp,
    toggle,
    select,
    clear
  };
}
