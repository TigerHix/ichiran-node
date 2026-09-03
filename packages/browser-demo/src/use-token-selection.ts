import { useCallback, useEffect, useState } from 'react';

export interface TokenSelection {
  readonly start: number;
  readonly end: number;
}

export function useTokenSelection(): {
  readonly selection: TokenSelection | null;
  readonly selecting: boolean;
  readonly pointerDown: (index: number) => void;
  readonly pointerEnter: (index: number) => void;
  readonly pointerUp: (index: number) => void;
  readonly toggle: (index: number) => void;
  readonly select: (index: number) => void;
  readonly clear: () => void;
} {
  const [selection, setSelection] = useState<TokenSelection | null>(null);
  const [anchor, setAnchor] = useState<number | null>(null);
  const [dragging, setDragging] = useState(false);

  const pointerDown = useCallback((index: number): void => {
    setAnchor(index);
    setDragging(false);
  }, []);

  const pointerEnter = useCallback((index: number): void => {
    if (anchor === null || index === anchor) return;
    setDragging(true);
    setSelection({ start: Math.min(anchor, index), end: Math.max(anchor, index) });
  }, [anchor]);

  const pointerUp = useCallback((index: number): void => {
    if (!dragging && anchor === index) {
      setSelection(current => current?.start === index && current.end === index
        ? null
        : { start: index, end: index });
    }
    setAnchor(null);
    setDragging(false);
  }, [anchor, dragging]);

  const clear = useCallback((): void => {
    setSelection(null);
    setAnchor(null);
    setDragging(false);
  }, []);

  const select = useCallback((index: number): void => {
    setSelection({ start: index, end: index });
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
