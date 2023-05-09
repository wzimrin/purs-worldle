export const drawShape = (context) => (shape) => {
  const unit = () => void 0;
  if (shape.length === 0) return unit;
  const [{ x, y }] = shape;
  context.beginPath();
  context.moveTo(x, y);
  shape.forEach(({ x, y }) => {
    context.lineTo(x, y);
  });
  context.fill();
  return unit;
};
